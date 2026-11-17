open! Hermetic
module M = Chaos_model
module Plan = Chaos_plan

type env = Chaos_env.env

module Op = Plan.Op
module Fault = Plan.Fault

(* {1 Wrapper} *)

let wrap f base =
  let hook ~space:_ op = f op in
  (M.wrap (M.schedule hook) base :> Eio.Fs.dir_ty Eio.Path.t)

(* {1 Runs} *)

(* The failures of one operation, while the clock stands still, that make a
   livelock: 10000 more than three times the first run's count on one path,
   or than 1000 times it for one kind in one directory, as a retry under fresh
   names makes. *)
let streak_limit tag first =
  10_000 + (first * match tag with `Path -> 3 | `Dir -> 1000)

(* A run that makes more operations spins, or is more than a check plans
   from: it stops there, so its operations stay bounded. *)
let max_ops = 100_000

(* How long an operation that meets no fault takes on the run's clock: a
   sync waits for the disk, another operation for the CPU. Other fibers run
   meanwhile. *)
let sync_time = Mtime.Span.(1 * ms)
let op_time = Mtime.Span.(1 * us)
let now_ns mono = Int64.to_int (Mtime.to_uint64_ns (Eio.Time.Mono.now mono))

(* [spend mono span] waits [span], and is how long the clock moved. *)
let spend mono span =
  let t0 = now_ns mono in
  Eio.Time.Mono.sleep_span mono span;
  now_ns mono - t0

type outcome =
  | Passed
  | Raised of exn
  | Deadlock
  | Time_limit
  | Busy
  | Too_long
  | Kept_going
  | Too_many
  | Livelock of [ `Path | `Dir ] * string * int
  | Over_budget of int (* the operation the first run stopped at *)
  | Restart of exn option * outcome
  (* what the faulty run raised, if anything, and how its restart failed *)
  | Lost of exn option * string
  (* what the faulty run raised, and the acknowledged file it changed *)
  | Inspected of exn option * exn
(* what the faulty run raised, and what inspect raised *)

(* A report a nested check raised is text, its lines set in. *)
let exn_text = function
  | Failure m when String.contains m '\n' ->
      String.concat "\n    " (String.split_on_char '\n' m)
  | x -> Printexc.to_string x

let rec outcome_text = function
  | Passed -> "none"
  | Raised x -> exn_text x
  | Deadlock -> "the run hangs: every fiber waits and no timer is due"
  | Time_limit ->
      "the run hangs: it is still running after an hour of its clock"
  | Busy ->
      "the run woke 10,000,000 times without waiting 1 ms or more: a busy loop"
  | Too_long ->
      "the run took 10,000,000 turns, more than a check can repeat: a busy \
       loop, or a workload too large for a check"
  | Kept_going ->
      "the run hangs: it kept going for 1000 operations after the power cut, \
       so it catches Eio.Cancel.Cancelled"
  | Too_many ->
      "the run made 100,000 filesystem operations, more than a check plans \
       from: a busy loop, or a workload too large for a check"
  | Livelock (`Path, path, n) ->
      Fmt.str
        "the run hangs: the same operation on %s failed %d times while its \
         clock stood still"
        path n
  | Livelock (`Dir, dir, n) ->
      Fmt.str
        "the run hangs: one kind of operation in %s failed %d times while its \
         clock stood still"
        dir n
  | Over_budget at ->
      Fmt.str "the linear tier needs more runs than the budget by operation #%d"
        at
  | Restart (_, o) -> outcome_text o
  | Lost (_, s) -> s
  | Inspected (_, x) -> "inspect raised " ^ exn_text x

(* A cut fails the run's switch with it, which cancels every fiber of the
   run. *)
exception Cut

(* {1 Acknowledged files} *)

(* The files a run acknowledged, each with what it held then, read beneath
   the fault wrapper. A file the run writes, truncates, renames or removes
   since is no longer compared. *)
module Ack = struct
  type entry = { path : string; held : string; at : int }

  type t = {
    base : Eio.Fs.dir_ty Eio.Path.t;
    at : unit -> int; (* the last operation's index *)
    mutable alive : unit -> bool; (* false once the power is cut *)
    mutable on_ack : unit -> unit;
    mutable live : entry list; (* latest first *)
    mutable paths : string list; (* latest first, each once *)
  }

  let v ~at base =
    {
      base :> Eio.Fs.dir_ty Eio.Path.t;
      at;
      alive = (fun () -> true);
      on_ack = ignore;
      live = [];
      paths = [];
    }

  (* The runs' envs, innermost first: a check may run inside another. *)
  let envs : (env * t) list ref Domain.DLS.key =
    Domain.DLS.new_key (fun () -> ref [])

  let within env t f =
    let l = Domain.DLS.get envs in
    l := (env, t) :: !l;
    Fun.protect ~finally:(fun () ->
        l := List.filter (fun (e, _) -> e != env) !l)
    @@ fun () -> f env

  let find name env =
    match List.assq_opt env !(Domain.DLS.get envs) with
    | Some t -> t
    | None -> Fmt.invalid_arg "%s: env is not that of a run of a check" name

  (* A path as the root names it, with no [""], ["."] or [".."]. *)
  let norm path =
    let rec go acc = function
      | [] -> List.rev acc
      | ("" | ".") :: l -> go acc l
      | ".." :: l -> go (match acc with [] -> [] | _ :: a -> a) l
      | c :: l -> go (c :: acc) l
    in
    String.concat "/" (go [] (String.split_on_char '/' path))

  let read t p =
    let f = Eio.Path.(t.base / p) in
    match Eio.Path.kind ~follow:false f with
    | `Regular_file -> Some (Eio.Path.load f)
    | _ -> None
    | exception Eio.Io _ -> None

  let ack name env ((_, path) : _ Eio.Path.t) =
    let t = find name env in
    if t.alive () then
      let p = norm path in
      match read t p with
      | None ->
          Fmt.invalid_arg "%s: %S is not a regular file of the run's filesystem"
            name path
      | Some held ->
          t.live <-
            { path = p; held; at = t.at () }
            :: List.filter (fun e -> e.path <> p) t.live;
          if not (List.mem p t.paths) then t.paths <- p :: t.paths;
          t.on_ack ()

  let acked name env = List.rev_map (fun p -> "/" ^ p) (find name env).paths

  let under ~dir p =
    dir = "" || p = dir || String.starts_with ~prefix:(dir ^ "/") p

  let touches (op : Op.t) p =
    match op.kind with
    | `Open_write | `Write | `Truncate -> norm op.path = p
    | `Change ->
        under ~dir:(norm op.path) p
        || Option.fold ~none:false
             ~some:(fun d -> under ~dir:(norm d) p)
             op.dest
    | `Open_read | `Stat | `Read | `Read_dir | `Sync -> false

  let touch t op =
    if t.live <> [] then
      t.live <- List.filter (fun e -> not (touches op e.path)) t.live

  let quote s =
    let n = String.length s in
    if n <= 40 then Fmt.str "%S" s
    else Fmt.str "%S... (%d bytes)" (String.sub s 0 32) n

  (* The first file acknowledged and untouched since that no longer holds
     what it held then. *)
  let lost t =
    List.find_map
      (fun e ->
        match read t e.path with
        | Some s when s = e.held -> None
        | now ->
            Fmt.kstr Option.some "/%s held %s when acknowledged %s, and now %s"
              e.path (quote e.held)
              (if e.at = 0 then "at the start" else Fmt.str "after #%d" e.at)
              (match now with
              | None -> "is missing"
              | Some s -> "holds " ^ quote s))
      (List.rev t.live)
end

(* The directory [d] opened as eio_posix opens a process's cwd: confined to
   itself, so an absolute path or link and a [..] above it are refused. It is
   opened beneath the fault wrapper, so the open is no operation of the run. *)
let confined ~sw (d : _ Eio.Path.t) =
  (Eio.Path.open_dir ~sw d :> Eio.Fs.dir_ty Eio.Path.t)

(* [f] on the env of [side] over [fs] and [cwd], whose acknowledgements go
   to [ack]. *)
let in_env ~side ?ack clock mono ~fs ~cwd f =
  let env = Chaos_env.v side clock mono ~fs ~cwd in
  Chaos_env.within side @@ fun () ->
  match ack with None -> f env | Some t -> Ack.within env t f

(* [f] on the env over [base], the unconfined root, wrapped by [wrap]. *)
let with_env ?(wrap = fun d -> (d :> Eio.Fs.dir_ty Eio.Path.t)) ?ack ~side clock
    mono (base : _ Eio.Path.t) f =
  Eio.Switch.run @@ fun sw ->
  let cwd = wrap (confined ~sw base) in
  in_env ~side ?ack clock mono
    ~fs:(wrap (base :> Eio.Fs.dir_ty Eio.Path.t))
    ~cwd f

(* After a run with a fault, before its restart: each acknowledged file
   still holds what it held, and [inspect] accepts the filesystem. *)
let settle ~side ~ack ~inspect ~ended clock mono base =
  match Ack.lost ack with
  | Some s -> Some (Lost (ended, s))
  | None -> (
      match with_env ~side ~ack clock mono base inspect with
      | () -> None
      | exception x -> Some (Inspected (ended, x)))

(* A run ends once [halted] holds why. *)
let in_loop ?idle ?effects ?(halted = ref None) main =
  match
    Quiet_mock.run ?idle ?effects
      ~stop:(fun () -> Option.is_some !halted)
      ~limit:3600.
      (fun clock mono _charge -> main clock mono)
  with
  | v -> Ok v
  | exception Quiet_mock.Stopped -> Error (Option.get !halted)
  | exception Quiet_mock.Deadlock_detected _ -> Error Deadlock
  | exception Quiet_mock.Time_limit -> Error Time_limit
  | exception Quiet_mock.Busy -> Error Busy
  | exception Quiet_mock.Too_long -> Error Too_long

(* Park the fiber where no cancel reaches, so even a loop that catches
   everything stops; the loop then reports a deadlock. *)
let park () = Eio.Cancel.protect Eio.Fiber.await_cancel

let halt halted o =
  halted := Some o;
  park ()

let count_op halted ops =
  incr ops;
  if !ops > max_ops then halt halted Too_many

(* The first run stops once the linear tier needs more runs than the
   budget: past that, the check cannot pass, and the run's operations need
   not grow. *)
let add_floor halted ~budget ~floor ~at n =
  floor := !floor + n;
  if !floor > budget then halt halted (Over_budget at)

let fresh ~os clock = (Memfs.v ~clock ~os () :> Eio.Fs.dir_ty Eio.Path.t)
let os_of = function Some `Macos -> `Macos | Some `Linux | None -> `Linux

let rec files (d : _ Eio.Path.t) prefix =
  List.concat_map
    (fun name ->
      let q = Eio.Path.(d / name) and path = prefix ^ "/" ^ name in
      match Eio.Path.kind ~follow:false q with
      | `Regular_file -> [ (path, String.length (Eio.Path.load q)) ]
      | `Directory -> files q path
      | _ -> [])
    (List.sort compare (Eio.Path.read_dir d))

(* {1 Reports, budget, replay} *)

let fail name fmt = Fmt.kstr (fun s -> failwith (name ^ ": " ^ s)) fmt

let budget_of name = function
  | None -> 20_000
  | Some b when b < 1 ->
      Fmt.invalid_arg "%s: budget is %d, must be positive" name b
  | Some b -> b

let parse_replay ?(well_formed = fun _ -> true) name r =
  let bad () =
    Fmt.invalid_arg "%s: replay is %S, not a replay string of eio-fs" name r
  in
  if not (String.starts_with ~prefix:"fs/" r) then bad ()
  else
    let r' = String.sub r 3 (String.length r - 3) in
    match String.index_opt r' '/' with
    | Some i when i > 0 && i < String.length r' - 1 ->
        let key = String.sub r' (i + 1) (String.length r' - i - 1) in
        if well_formed key then (String.sub r' 0 i, key) else bad ()
    | _ -> bad ()

let digest (trace : Plan.trace) =
  let s = Fmt.to_to_string Fmt.(list ~sep:(any ";") Op.pp) trace in
  String.sub (Digest.to_hex (Digest.string s)) 0 8

(* Two runs with no fault, [a] and [b], must make the same kinds of
   operation in the same order, for a fault to find its operation again;
   the names they give their files may differ. *)
let same_kinds name (a : Plan.trace) (b : Plan.trace) =
  let rec first = function
    | (x : Op.t) :: a, (y : Op.t) :: b ->
        if x.kind = y.kind then first (a, b) else Some (Some x, Some y)
    | x :: _, [] -> Some (Some x, None)
    | [], y :: _ -> Some (None, Some y)
    | [], [] -> None
  in
  let op ppf = function
    | Some op -> Op.pp ppf op
    | None -> Fmt.string ppf "nothing more"
  in
  match first (a, b) with
  | None -> ()
  | Some (x, y) ->
      fail name
        "two runs with no fault made different operations: %a in the first, %a \
         in the second. A check aims each fault at an operation by its kind \
         and its place in the run, so it cannot aim them here. The program \
         reached something the check does not simulate: Random.self_init, a \
         process id, the wall clock read through Unix, a second domain, or \
         state an earlier run left in memory. Built with hermetic (open! \
         Hermetic), the first four do not compile."
        op x op y

let parent path =
  match String.rindex_opt path '/' with
  | Some 0 -> "/"
  | Some i -> String.sub path 0 i
  | None -> "."

(* What a repeated operation keeps: its kind, the directory it is in and
   its fault, so a retry under one name or under fresh ones repeats. *)
let shape ((op : Op.t), fault) = (op.kind, parent op.path, fault)

(* The lines of a trace: an operation, or a block of [period] operations
   repeated, of which the first and the last are shown. *)
type line =
  | Line of (Op.t * Fault.t option)
  | Repeated of { period : int; times : int; first : int; last : int }

let max_period = 8
let min_times = 3

(* A trace shows at most [keep] lines at each end, and those around the
   first fault. *)
let keep = 100
let context = 10

(* A run's trace as its report shows it, kept as the run goes. At each
   place, the block of up to [max_period] operations repeated [min_times]
   times or more that covers the most operations, the shortest on a tie,
   collapses to its first and last instances. Of the lines that leaves, only
   the first and last [window] and those around the first fault are kept, so
   a run that spins costs no more than one that does not. *)
module Recorder = struct
  type entry = (Op.t * Fault.t option) * (Op.kind * string * Fault.t option)

  (* A block repeating at the end of the trace: its first instance, the
     instances so far, the last whole one and the start of the next. *)
  type rep = {
    period : int;
    first_block : entry array;
    second : int; (* the first operation of the second instance *)
    mutable count : int;
    mutable last_block : entry array;
    mutable before_last : int; (* the operation before the last instance *)
    mutable partial : entry list; (* the next instance, latest first *)
    mutable npartial : int;
  }

  let window = (2 * keep) + (2 * context) + 1
  let blank = Repeated { period = 0; times = 0; first = 0; last = 0 }

  type t = {
    mutable buf : entry array; (* the operations no line holds yet *)
    mutable len : int;
    mutable rep : rep option;
    head : line array; (* the first [window] lines *)
    ring : line array; (* the last [window] lines *)
    mutable n : int;
    mutable fault : int option; (* the line of the first fault *)
    mutable around : (int * line) list;
  }

  let v () =
    {
      buf = [||];
      len = 0;
      rep = None;
      head = Array.make window blank;
      ring = Array.make window blank;
      n = 0;
      fault = None;
      around = [];
    }

  let index ((((op : Op.t), _), _) : entry) = op.index

  let emit t l =
    let k = t.n in
    if k < window then t.head.(k) <- l;
    t.ring.(k mod window) <- l;
    (match (t.fault, l) with
    | None, Line (_, Some _) ->
        t.fault <- Some k;
        for j = max 0 (k - context) to k do
          t.around <- (j, t.ring.(j mod window)) :: t.around
        done
    | Some f, _ when k <= f + context -> t.around <- (k, l) :: t.around
    | _ -> ());
    t.n <- k + 1

  let emit_block t b = Array.iter (fun (e, _) -> emit t (Line e)) b

  let append t e =
    if t.len = Array.length t.buf then begin
      let b = Array.make (max 16 (2 * t.len)) e in
      Array.blit t.buf 0 b 0 t.len;
      t.buf <- b
    end;
    t.buf.(t.len) <- e;
    t.len <- t.len + 1

  let drop t k =
    Array.blit t.buf k t.buf 0 (t.len - k);
    t.len <- t.len - k

  (* The whole instances of the block of [p] operations from the start of
     the buffer, and whether more may come. *)
  let run t p =
    let rec go r =
      let j = r * p in
      let rec same k =
        k >= p
        || j + k >= t.len
        || (snd t.buf.(j + k) = snd t.buf.(j + k - p) && same (k + 1))
      in
      if not (same 0) then (r, false)
      else if j + p > t.len then (r, true)
      else go (r + 1)
    in
    go 1

  let close t rep rest =
    t.rep <- None;
    emit_block t rep.first_block;
    emit t
      (Repeated
         {
           period = rep.period;
           times = rep.count - 2;
           first = rep.second;
           last = rep.before_last;
         });
    emit_block t rep.last_block;
    List.iter (append t) rest

  (* Decide the lines the buffer starts with, as far as the operations so
     far allow, or all of them at the end. *)
  let rec decide ~at_end t =
    if t.len > 0 then begin
      let runs = Array.init max_period (fun k -> run t (k + 1)) in
      let ongoing k = (not at_end) && snd runs.(k) in
      let best ~only =
        let bp = ref 1 and br = ref 1 in
        Array.iteri
          (fun k (r, _) ->
            let p = k + 1 in
            if only k && r >= min_times && r * p > !bp * !br then begin
              bp := p;
              br := r
            end)
          runs;
        (!bp, !br)
      in
      match List.find_opt ongoing (List.init max_period Fun.id) with
      | None ->
          let p, r = best ~only:(fun _ -> true) in
          if r >= min_times then begin
            let b = Array.sub t.buf 0 (r * p) in
            emit_block t (Array.sub b 0 p);
            emit t
              (Repeated
                 {
                   period = p;
                   times = r - 2;
                   first = index b.(p);
                   last = index b.(((r - 1) * p) - 1);
                 });
            emit_block t (Array.sub b ((r - 1) * p) p);
            drop t (r * p)
          end
          else begin
            emit t (Line (fst t.buf.(0)));
            drop t 1
          end;
          decide ~at_end t
      | Some k ->
          (* Past [2 * max_period] operations, every block still repeating
             is a multiple of the shortest one, which covers as much as
             they do: once it covers more than any that ended, it wins. *)
          let q = k + 1 and r = fst runs.(k) in
          let bp, br = best ~only:(fun k -> not (ongoing k)) in
          if t.len >= 2 * max_period && r >= min_times && r * q > bp * br then begin
            let rest =
              Array.to_list (Array.sub t.buf (r * q) (t.len - (r * q)))
            in
            t.rep <-
              Some
                {
                  period = q;
                  first_block = Array.sub t.buf 0 q;
                  second = index t.buf.(q);
                  count = r;
                  last_block = Array.sub t.buf ((r - 1) * q) q;
                  before_last = index t.buf.(((r - 1) * q) - 1);
                  partial = List.rev rest;
                  npartial = List.length rest;
                };
            t.len <- 0;
            t.buf <- [||]
          end
    end

  let add t ((op, fault) as e) =
    let e = (e, shape (op, fault)) in
    match t.rep with
    | None ->
        append t e;
        decide ~at_end:false t
    | Some rep ->
        let k = rep.npartial in
        if snd e = snd rep.last_block.(k) then begin
          rep.partial <- e :: rep.partial;
          rep.npartial <- k + 1;
          if k + 1 = rep.period then begin
            rep.before_last <- index rep.last_block.(rep.period - 1);
            rep.last_block <- Array.of_list (List.rev rep.partial);
            rep.partial <- [];
            rep.npartial <- 0;
            rep.count <- rep.count + 1
          end
        end
        else begin
          close t rep (List.rev (e :: rep.partial));
          decide ~at_end:false t
        end

  let finish t =
    (match t.rep with
    | Some rep -> close t rep (List.rev rep.partial)
    | None -> ());
    decide ~at_end:true t

  let line t i =
    if i < window then t.head.(i)
    else if i >= t.n - window then t.ring.(i mod window)
    else List.assoc i t.around

  (* The report's lines: the first and last [keep] and those around the
     first fault, or all of them when they are few. *)
  let pp ppf t =
    finish t;
    let n = t.n in
    let shown i =
      n <= window || i < keep
      || i >= n - keep
      || match t.fault with Some f -> abs (i - f) <= context | None -> false
    in
    let plural k = if k = 1 then "" else "s" in
    let rec pp i =
      if i < n then (
        if shown i then begin
          (match line t i with
          | Line (op, None) -> Fmt.pf ppf "    %a@\n" Op.pp op
          | Line (op, Some f) ->
              Fmt.pf ppf "    %a  <- %a@\n" Op.pp op Fault.pp f
          | Repeated { period; times; first; last } ->
              Fmt.pf ppf
                "    ... the %d operation%s above, %d more time%s, #%d to #%d@\n"
                period (plural period) times (plural times) first last);
          pp (i + 1)
        end
        else
          let rec skip j = if j < n && not (shown j) then skip (j + 1) else j in
          let j = skip i in
          Fmt.pf ppf "    ... %d more line%s@\n" (j - i) (plural (j - i));
          pp j)
    in
    pp 0
end

(* What a run keeps of its operations: the report's lines, its first [cap]
   operations, which the plan of the next scenarios reads, and those at
   [marked] places, which the report names. *)
type record = {
  side : Chaos_env.side; (* the envs of the runs, and what they printed *)
  lines : Recorder.t;
  mutable kept : (Op.t * Fault.t option) list; (* latest first *)
  mutable nkept : int;
  cap : int;
  marked : Op.t -> bool;
  mutable marks : Op.t list;
  mutable last : int; (* the last operation's index *)
}

let record ?(cap = max_int) ?(marked = fun _ -> false) () =
  {
    side = Chaos_env.side ();
    lines = Recorder.v ();
    kept = [];
    nkept = 0;
    cap;
    marked;
    marks = [];
    last = 0;
  }

let note r ((op : Op.t), fault) =
  Recorder.add r.lines (op, fault);
  r.last <- op.index;
  if r.nkept < r.cap then begin
    r.kept <- (op, fault) :: r.kept;
    r.nkept <- r.nkept + 1
  end;
  if r.marked op then r.marks <- op :: r.marks

(* The operations kept, in order. *)
let kept r = List.rev_map fst r.kept

(* The operation at [p], where the report names it. *)
let found r p =
  match List.find_opt (Plan.at p) r.marks with
  | Some op -> Some op
  | None -> List.find_opt (Plan.at p) (kept r)

(* What the program printed, a line of a report, its lines set in. *)
let output (trace : record) =
  let s = String.trim (Chaos_env.printed trace.side) in
  ("output", String.concat "\n            " (String.split_on_char '\n' s))

(* The report of a failing run: the scenario's lines, the trace, and the
   replay that runs it alone. *)
let report name ~title ~hash ~key ~matched ~(trace : record) ~error lines =
  let matched =
    Option.map
      (fun old ->
        Fmt.str "the replay named first run %s, and the workload's is now %s"
          old hash)
      matched
  in
  let lines =
    lines
    @ Option.fold ~none:[] ~some:(fun m -> [ ("matched", m) ]) matched
    @ [ ("error", error); output trace ]
  in
  let line ppf (k, v) = if v <> "" then Fmt.pf ppf "  %-9s %s@\n" (k ^ ":") v in
  let replay = Fmt.str "fs/%s/%s" hash key in
  Fmt.failwith "%s: %s@\n%a  trace:@\n%a  replay:   %s ~replay:%S app" name
    title
    Fmt.(list ~sep:nop line)
    lines Recorder.pp trace.lines name replay

(* The first run failed, or stopped at the budget: its trace says where. *)
let refuse_first name ~(trace : record) out ~budget ~what =
  let head ppf = function
    | Over_budget at ->
        Fmt.pf ppf
          "%s: the linear tier needs more than the budget of %d runs by \
           operation #%d of the first run, which stopped there, and a check \
           never passes on a partial one. Shrink the workload, or raise \
           ~budget."
          name budget at
    | o ->
        Fmt.pf ppf "%s: the run with no %s failed: %s" name what
          (outcome_text o)
  in
  let line ppf (k, v) = if v <> "" then Fmt.pf ppf "  %-9s %s@\n" (k ^ ":") v in
  Fmt.kstr
    (fun report -> failwith (String.trim report))
    "%a@\n%a  trace:@\n%a" head out line (output trace) Recorder.pp trace.lines

let no_longer name ~hash (h, k) =
  if h = hash then
    fail name "the replay names %s, a scenario this check no longer has" k
  else
    fail name
      "the replay names %s, a scenario this check no longer has: its first run \
       was %s and is now %s. Run the check without ~replay."
      k h hash

(* {1 check} *)

(* An operation: its kind and the path it names, or the directory that
   path is in. *)
type target = Op.kind * [ `Path | `Dir ] * string

let targets (op : Op.t) =
  [ (op.kind, `Path, op.path); (op.kind, `Dir, parent op.path) ]

(* How often the first run made each operation. *)
let made (trace : Plan.trace) =
  let t = Hashtbl.create 16 in
  List.iter
    (fun op ->
      List.iter
        (fun k ->
          Hashtbl.replace t k
            (1 + Option.value ~default:0 (Hashtbl.find_opt t k)))
        (targets op))
    trace;
  t

(* Injected failures of each operation while the run's clock stands still,
   since its last attempt that met no fault. The time operations took does
   not move it: a retry spins whatever operations it makes between two
   failures. *)
type streak = {
  counts : (target, int) Hashtbl.t;
  first : (target, int) Hashtbl.t;
  mutable at : int;
  mutable hit : ([ `Path | `Dir ] * string * int) option;
  mutable mono : Eio.Time.Mono.ty Eio.Resource.t option;
  mutable taken : int; (* the time operations took, in ns *)
}

let streak first =
  {
    counts = Hashtbl.create 16;
    first;
    at = min_int;
    hit = None;
    mono = None;
    taken = 0;
  }

let take_time st (op : Op.t) =
  match st.mono with
  | Some m ->
      let span = if op.kind = `Sync then sync_time else op_time in
      st.taken <- st.taken + spend m span
  | None -> ()

let failed st (op : Op.t) =
  let now = match st.mono with Some m -> now_ns m - st.taken | None -> 0 in
  if now <> st.at then begin
    st.at <- now;
    Hashtbl.reset st.counts
  end;
  List.iter
    (fun ((_, tag, at) as key) ->
      let n = 1 + Option.value ~default:0 (Hashtbl.find_opt st.counts key) in
      Hashtbl.replace st.counts key n;
      let limit =
        streak_limit tag
          (Option.value ~default:0 (Hashtbl.find_opt st.first key))
      in
      if n >= limit then begin
        st.hit <- Some (tag, at, limit);
        if n >= 2 * limit then park ();
        failwith
          ("Eio_fs_chaos.run: " ^ outcome_text (Livelock (tag, at, limit)))
      end)
    (targets op)

let met_no_fault st (op : Op.t) =
  if Hashtbl.length st.counts > 0 then
    List.iter (Hashtbl.remove st.counts) (targets op)

(* The fault scenario [s] puts on [op]: a lasting fault from its point on, or
   the next pending one placed at [op]. *)
let scheduled_fault (s : Plan.check) ~on ~pending op =
  match s.lasting with
  | Some (l, p) ->
      if Plan.at p op then on := true;
      if !on then Option.map (fun c -> Fault.Fail c) (Plan.lasting_code l op)
      else None
  | None -> (
      match List.partition (fun (p, _) -> Plan.at p op) !pending with
      | (_, f) :: _, rest ->
          pending := rest;
          Some f
      | [], _ -> None)

(* A faulty run may end with [Eio.Io], from one fiber or several. *)
let rec is_io = function
  | Eio.Io _ -> true
  | Eio.Exn.Multiple l -> l <> [] && List.for_all (fun (x, _) -> is_io x) l
  | _ -> false

(* Runs [setup] then [run] over a fresh base wrapped by [hook], on a disk
   model when a fault of [s] evicts pages. A run with a fault that returns or
   raises [Eio.Io] sets [ended] and is followed by [run] again on the same
   base, its operations shown to [again] alone. *)
let run_scenario ~side ~halted ~ended ~again ~os ~setup ~inspect ~ack ~hook ~st
    run (s : Plan.check) =
  let faulty = s.faults <> [] || s.lasting <> None in
  match
    in_loop ~halted @@ fun clock mono ->
    st.mono <- Some mono;
    let base = fresh ~os clock in
    with_env ~side clock mono base setup;
    (* Pages a failed sync evicts read as the disk holds them, which the
       model of the disk knows. *)
    let base =
      if List.exists (function _, Fault.Evict _ -> true | _ -> false) s.faults
      then (M.Crash.wrap (M.Crash.v ~os ()) base :> Eio.Fs.dir_ty Eio.Path.t)
      else base
    in
    let a = ack base in
    (* [fs] and [cwd] share one numbering of the operations. *)
    let on hook =
      let sched = M.schedule hook in
      fun d -> (M.wrap sched d :> Eio.Fs.dir_ty Eio.Path.t)
    in
    let restart e =
      match settle ~side ~ack:a ~inspect ~ended:e clock mono base with
      | Some o -> o
      | None -> (
          ended := Some e;
          Chaos_env.mark side "-- restart --";
          match with_env ~side ~wrap:(on again) ~ack:a clock mono base run with
          | () -> Passed
          | exception x -> Raised x)
    in
    match with_env ~side ~wrap:(on hook) ~ack:a clock mono base run with
    | () when faulty -> restart None
    | () -> Passed
    | exception x when faulty && is_io x -> restart (Some x)
    | exception x -> Raised x
  with
  | Ok o -> o
  | Error o -> o

(* The acknowledgements of a run whose operations go to [trace]: how an
   operation touches them, and how a run over a base makes them. *)
let acks trace =
  let acks = ref None in
  let touch op = Option.iter (fun a -> Ack.touch a op) !acks in
  let ack base =
    let a = Ack.v ~at:(fun () -> trace.last) base in
    acks := Some a;
    a
  in
  (touch, ack)

(* A scenario's outcome: why its loop halted, a livelock its streak met, or
   how its run ended, each a failed restart once the run with a fault
   ended. *)
let verdict ~halted ~ended ~st outcome =
  let restarted o =
    match !ended with Some e when o <> Passed -> Restart (e, o) | _ -> o
  in
  match (!halted, st.hit) with
  | Some o, _ -> restarted o
  | None, Some (t, p, n) -> Livelock (t, p, n)
  | None, None -> restarted outcome

let exec_check ?spaces ?cap ?budget ?(inspect = ignore) ~os ~first ~setup run
    (s : Plan.check) =
  let marked op =
    List.exists (fun (p, _) -> Plan.at p op) s.faults
    || match s.lasting with Some (_, p) -> Plan.at p op | None -> false
  in
  let trace = record ?cap ~marked () in
  let pending = ref s.faults and on = ref false and st = streak first in
  let halted = ref None and floor = ref 0 and ops = ref 0 in
  let ended = ref None and sure = Plan.Sure.v () in
  let touch, ack = acks trace in
  let hook ~space (op : Op.t) =
    count_op halted ops;
    touch op;
    Option.iter
      (fun budget ->
        add_floor halted ~budget ~floor ~at:op.index
          (Plan.check_floor ~space ~sure:(Plan.Sure.add sure op) op))
      budget;
    Option.iter
      (fun t -> Hashtbl.replace t { Plan.kind = op.kind; rank = op.rank } space)
      spaces;
    let fault =
      Option.bind (scheduled_fault s ~on ~pending op) (fun f ->
          if M.applies ~space op f then Some f else None)
    in
    note trace (op, fault);
    match fault with
    | Some _ ->
        failed st op;
        fault
    | None ->
        met_no_fault st op;
        take_time st op;
        None
  in
  let restart_ops = ref 0 in
  let again ~space:_ (op : Op.t) =
    count_op halted restart_ops;
    touch op;
    take_time st op;
    None
  in
  let outcome =
    run_scenario ~side:trace.side ~halted ~ended ~again ~os ~setup ~inspect ~ack
      ~hook ~st run s
  in
  (trace, verdict ~halted ~ended ~st outcome)

(* The scenario a replay's key [k] names: a linear one, another single
   fault, or a second fault a linear one's run allows. *)
let replayed ~exec ~meets ~rng ~disk ~first linear k =
  let find l = List.find_opt (fun s -> Plan.check_key s = k) l in
  match find linear with
  | Some _ as s -> s
  | None -> (
      match find (List.filter meets (Plan.check_others first)) with
      | Some _ as s -> s
      | None ->
          List.find_map
            (fun (s : Plan.check) ->
              match s.faults with
              | [ one ]
                when String.starts_with ~prefix:(Plan.check_key s ^ "+") k ->
                  let r = fst (exec s) in
                  let changed, rest =
                    Plan.check_seconds ~rng one ~first:disk ~after:(kept r)
                  in
                  find (changed @ rest)
              | _ -> None)
            linear)

(* The linear tier whole, then the sampled one: other single faults and the
   best second faults the linear runs allowed, each class with its share of
   the budget. *)
let run_tiers name ~budget ~rng ~meets ~disk ~first ~attempt linear =
  let n = List.length linear in
  (* Evictions are linear runs the sampled tier does not grow with. *)
  let n_share =
    List.length
      (List.filter
         (fun (s : Plan.check) -> not (List.exists Plan.is_evict s.faults))
         linear)
  in
  if n > budget then
    fail name
      "the linear tier needs %d runs, more than the budget of %d, and a check \
       never passes on a partial one. Shrink the workload, or raise ~budget."
      n budget;
  let others, pick_others = Plan.check_others_sample ~keep:meets first in
  let share classes = Plan.sampled_share ~budget ~linear:n_share ~classes in
  (* The second faults each run allows, drawn as it ends, so no run's
     operations outlive it: the best of each, round by round. *)
  let sh = share (if others = 0 then 1 else 2) in
  let best = Plan.Pairs.v sh in
  List.iter
    (fun (s : Plan.check) ->
      let r = attempt s in
      match s.faults with
      | [ one ] when not (Plan.is_evict one) ->
          Plan.Pairs.add best
            (Plan.check_seconds ~limit:sh ~rng one ~first:disk ~after:(kept r))
      | _ -> ())
    linear;
  let pairs = Plan.Pairs.to_list best in
  let classes =
    List.filter
      (fun (n, _) -> n > 0)
      [
        (others, pick_others rng);
        (List.length pairs, fun n -> List.take n pairs);
      ]
  in
  let share = share (List.length classes) in
  List.iter
    (fun (_, pick) ->
      List.iter (fun s -> ignore (attempt s : record)) (pick share))
    classes

(* A scenario whose fault the operation never meets is none: an [ENOSPC] on
   an operation that takes no space. *)
let meets spaces (s : Plan.check) =
  List.for_all
    (fun (p, f) ->
      match f with
      | Fault.Fail `Enospc -> Hashtbl.find_opt spaces p <> Some false
      | _ -> true)
    s.faults

(* The report of a scenario whose run failed, or whose restart did. *)
let failed_check name ~hash ~matched ~trace s out =
  let faults = ("faults", Plan.check_text ~at:(found trace) s) in
  let report = report name ~hash ~key:(Plan.check_key s) ~matched ~trace in
  let ended e =
    ( "ended",
      match e with
      | None -> "the faulty run returned"
      | Some x -> "the faulty run raised " ^ exn_text x )
  in
  match out with
  | Restart (e, o) ->
      report
        ~title:
          "the restart failed: the workload, run again with no fault on what \
           the faulty run left, did not return"
        ~error:(outcome_text o)
        [ faults; ended e ]
  | Lost (e, _) ->
      report
        ~title:
          "an acknowledged file lost what it held: after the fault, before the \
           restart, it no longer holds what it held at its ack"
        ~error:(outcome_text out)
        [ faults; ended e ]
  | Inspected (e, _) ->
      report ~title:"inspect rejected what the fault left"
        ~error:(outcome_text out)
        [ faults; ended e ]
  | o ->
      report ~title:"the workload failed under a fault" ~error:(outcome_text o)
        [ faults ]

let check_faults ~name ?budget ?(seed = 0) ?replay ?os ?(setup = ignore)
    ?inspect run =
  let os = os_of os in
  let rng = Random.State.make [| seed |] in
  let budget = budget_of name budget in
  let replay = Option.map (parse_replay name) replay in
  let spaces = Hashtbl.create 16 in
  (* A replay runs one scenario, whatever the budget. *)
  let first, out =
    exec_check ~spaces ~os
      ?budget:(if replay = None then Some budget else None)
      ~first:(Hashtbl.create 1) ~setup run Plan.none
  in
  if out <> Passed then refuse_first name ~trace:first out ~budget ~what:"fault";
  let first = kept first in
  if first = [] then
    fail name "the workload makes no filesystem operation, so it tests nothing";
  (* A run keeps no more operations than four times the first run made:
     past that, the plan of the next scenarios reads its first ones. *)
  let n = List.length first in
  let exec ?(cap = (4 * n) + 10_000) s =
    exec_check ~cap ?inspect ~os ~first:(made first) ~setup run s
  in
  same_kinds name first (kept (fst (exec ~cap:(n + 1) Plan.none)));
  let hash = digest first in
  let disk = Plan.Disk.v first in
  let meets = meets spaces in
  let linear = List.filter meets (Plan.check_linear first) in
  let attempt ?matched s =
    let trace, out = exec s in
    if out <> Passed then failed_check name ~hash ~matched ~trace s out;
    trace
  in
  match replay with
  | Some ((h, k) as r) -> (
      (* After a fix the first run may differ: the scenario is found by the
         operations it names, which its key carries. *)
      match replayed ~exec ~meets ~rng ~disk ~first linear k with
      | Some s ->
          let matched = if h = hash then None else Some h in
          ignore (attempt ?matched s : record)
      | None -> no_longer name ~hash r)
  | None ->
      run_tiers name ~budget ~rng ~meets ~disk ~first
        ~attempt:(fun s -> attempt s)
        linear

(* {1 Crash} *)

module Crash = struct
  module Unsynced = struct
    type kind = M.Crash.op_kind =
      | Data
      | Truncate
      | Create
      | Mkdir
      | Unlink
      | Rmdir
      | Symlink of { target : string }
      | Rename of { dest : string }
      | Chmod of { perm : int }
      | Chown of { uid : int64; gid : int64 }

    type t = M.Crash.unsynced = {
      path : string;
      kind : kind;
      file_offset : int;
      length : int;
      seq : int;
    }

    let pp = M.Crash.pp_unsynced
  end

  type unsynced = Unsynced.t
  type policy = Drop_unsynced | Survive of (Unsynced.t -> bool)
  type t = M.Crash.t

  (* A controller built by hand may wrap a directory other processes write,
     so its cut keeps their changes. A check reaches its directory through
     the wrapper alone and builds its own on [Dropped]. *)
  let v ?os = function
    | Drop_unsynced -> M.Crash.v ?os ~policy:Drop_unsynced ~outside:Kept ()
    | Survive p -> M.Crash.v ?os ~policy:(Survive p) ~outside:Kept ()

  let wrap t base = (M.Crash.wrap t base :> Eio.Fs.dir_ty Eio.Path.t)
  let unsynced = M.Crash.unsynced
  let cut = M.Crash.post_crash

  (* {2 The check} *)

  (* The draws a state makes go to [trail], each with the choice it names,
     out of range or not, and how many it had. *)
  let policy_of trail : Plan.state -> M.Crash.policy = function
    | Drop -> Drop_unsynced
    | Keep -> Survive (fun _ -> true)
    | Enum v | Shape v ->
        let v = Array.of_list v and pos = ref 0 in
        Choose
          (fun n ->
            let c = if !pos < Array.length v then v.(!pos) else 0 in
            incr pos;
            trail := (c, n) :: !trail;
            if 0 <= c && c < n then c else 0)
    | Seed s ->
        let rng = Random.State.make [| s |] in
        Choose
          (fun n ->
            let c = Random.State.int rng n in
            trail := (c, n) :: !trail;
            c)

  (* Whether the cut allows [st], given the draws it made: no choice out of
     range, and no choice for a draw it did not make. *)
  let allows (st : Plan.state) trail =
    match st with
    | Enum v | Shape v ->
        List.for_all (fun (c, n) -> 0 <= c && c < n) trail
        && List.length v <= List.length trail
    | Drop | Keep | Seed _ -> true

  type run = {
    trace : record;
    cut_op : Op.t option; (* the operation the cut fell before *)
    seqs : (int * int) list;
        (* for each operation, latest first, the last change before it *)
    rtrace : Plan.trace;
    changed : (int, unit) Hashtbl.t;
        (* the syncs and opens that changed what a cut leaves, and the
           operations an ack followed *)
    rchanged : (int, unit) Hashtbl.t; (* the same in recover *)
    acked_at : int list; (* the operation each ack came after *)
    lost : unsynced list;
    left : string list;
    trail : (int * int) list;
    allowed : bool;
    error : error option;
  }

  and error =
    | Workload of outcome
    | Settle of outcome (* an acknowledged file lost, or inspect raised *)
    | Recover of outcome
    | Again of exn

  (* A phase of a run: [f] on the crash-tracked [fs], whose power is cut
     before the operation at [cut], if it comes. It is the outcome and the
     unsynced changes the cut lost. The syncs and opens that changed the
     controller go to [changed]. An operation that meets no fault takes its
     time before its effect, and a sync in the middle of its own, through
     [pause], which the controller runs. *)
  let phase ?(guard = ignore) ~touch ~kept_going ~trace ~ctl ~pause ~mono
      ~faults ~cut ~cut_op ~seqs ~changed ~stop fs f =
    let pending = ref faults in
    let after = ref 0 and lost = ref [] and run_sw = ref None in
    (* The last sync or open, and the controller's stamp before its effect,
       which follows with no other operation in between. *)
    let sync = ref None in
    let synced () =
      match !sync with
      | Some (i, s) ->
          sync := None;
          if M.Crash.stamp ctl <> s then Hashtbl.replace changed i ()
      | None -> ()
    in
    let cancelled () =
      (* The operation the cut falls before, and every one after, is
         cancelled; once every fiber waits, so is the run. One that catches
         the cancellation still reaches this hook, and is stopped. *)
      incr after;
      if !after > 1000 then begin
        kept_going := true;
        park ()
      end;
      raise (Eio.Cancel.Cancelled Cut)
    in
    (* The operation about to have its effect. *)
    let landing (op : Op.t) ~tracked =
      if M.Crash.is_crashed ctl then cancelled ();
      synced ();
      if tracked then sync := Some (op.index, M.Crash.stamp ctl);
      seqs :=
        match !seqs with
        | (s, _) :: rest when s = M.Crash.seq ctl -> (s, op.index) :: rest
        | l -> (M.Crash.seq ctl, op.index) :: l
    in
    (* The sync whose time the controller takes next. *)
    let syncing = ref None in
    (pause :=
       fun () ->
         let op = !syncing in
         syncing := None;
         ignore (spend mono sync_time : int);
         Option.iter (landing ~tracked:true) op);
    let hook ~space (op : Op.t) =
      if not (M.Crash.is_crashed ctl) then synced ();
      guard op;
      if not (M.Crash.is_crashed ctl) then
        begin match cut with
        | Some p when Plan.at p op ->
            cut_op := Some op;
            lost := M.Crash.unsynced ctl;
            M.Crash.crash ctl;
            stop := !run_sw
        | _ -> ()
        end;
      if M.Crash.is_crashed ctl then cancelled ();
      touch op;
      let tracked = op.kind = `Sync || op.kind = `Open_write in
      match List.partition (fun (p, _) -> Plan.at p op) !pending with
      | (_, f) :: _, rest ->
          pending := rest;
          let m = if M.applies ~space op f then Some f else None in
          note trace (op, m);
          landing op ~tracked;
          m
      | [], _ ->
          note trace (op, None);
          if op.kind = `Sync then begin
            syncing := Some op;
            landing op ~tracked:false
          end
          else begin
            ignore (spend mono op_time : int);
            landing op ~tracked
          end;
          None
    in
    let sched = M.schedule hook in
    let wrapped = M.wrap sched fs in
    let outcome =
      match
        Eio.Switch.run (fun sw ->
            run_sw := Some sw;
            f wrapped (M.wrap sched (confined ~sw fs));
            if not (M.Crash.is_crashed ctl) then synced ())
      with
      | () ->
          stop := None;
          Passed
      | exception _ when M.Crash.is_crashed ctl ->
          stop := None;
          Passed
      | exception x -> Raised x
    in
    let outcome = if !kept_going then Kept_going else outcome in
    (outcome, !lost)

  let exec ?cap ?budget ?(inspect = ignore) ~os ~setup run ~recover
      (s : Plan.crash) =
    let trail = ref [] and rtrail = ref [] in
    let acked = ref [] and acked_at = ref [] in
    let marked op = List.exists (fun (p, _) -> Plan.at p op) s.cfaults in
    let trace = record ?cap ~marked () and rtrace = record ?cap () in
    let lost = ref [] and left = ref [] in
    let cut_op = ref None and seqs = ref [] in
    let changed = Hashtbl.create 16 and rchanged = Hashtbl.create 16 in
    let kept_run = ref false and kept_recover = ref false in
    let recovering = ref false in
    let halted = ref None and floor = ref 0 and ops = ref 0 in
    (* The operation before, once the one after it shows it is no cut at the
       end, and whether it is sure to be structural. *)
    let sure = Plan.Sure.v () and prev = ref None in
    (* A cut stops the run, as nothing runs after a power cut: once every
       fiber waits, the phase's switch fails, so a fiber that waits on a
       timer or another fiber stops too. *)
    let stop = ref None in
    let idle () =
      match !stop with
      | Some sw ->
          stop := None;
          Eio.Switch.fail sw Cut;
          true
      | None -> false
    in
    let error =
      match
        in_loop ~idle ~halted @@ fun clock mono ->
        let side = trace.side in
        let base = fresh ~os clock in
        with_env ~side clock mono base setup;
        let pause = ref ignore in
        let ctl =
          M.Crash.v ~os ~policy:(policy_of trail s.state) ~block_size:512
            ~pause:(fun () -> !pause ())
            ()
        in
        let fs = M.Crash.wrap ctl base in
        let ack = Ack.v ~at:(fun () -> trace.last) base in
        ack.alive <- (fun () -> not (M.Crash.is_crashed ctl));
        (* A file's ack changes what a cut must keep, as a value's does. *)
        ack.on_ack <-
          (fun () ->
            Hashtbl.replace changed trace.last ();
            acked_at := trace.last :: !acked_at);
        let touch = Ack.touch ack in
        let settle () =
          Option.map
            (fun o -> Settle o)
            (settle ~side ~ack ~inspect ~ended:None clock mono base)
        in
        let out, l =
          phase ~touch
            ~guard:(fun op ->
              count_op halted ops;
              Option.iter
                (fun budget ->
                  Option.iter
                    (fun ((p : Op.t), s) ->
                      add_floor halted ~budget ~floor ~at:op.index
                        (Plan.crash_floor ~sure:s
                           ~changed:(Hashtbl.mem changed p.index)
                           p))
                    !prev;
                  prev := Some (op, Plan.Sure.add sure op))
                budget)
            ~kept_going:kept_run ~trace ~ctl ~pause ~mono ~faults:s.cfaults
            ~cut:s.cut ~cut_op ~seqs ~changed ~stop fs
            (fun fs cwd ->
              in_env ~side ~ack clock mono ~fs ~cwd
                (run ~ack:(fun v ->
                     if not (M.Crash.is_crashed ctl) then begin
                       Hashtbl.replace changed trace.last ();
                       acked := v :: !acked;
                       acked_at := trace.last :: !acked_at
                     end)))
        in
        if not (M.Crash.is_crashed ctl) then begin
          lost := M.Crash.unsynced ctl;
          M.Crash.crash ctl
        end
        else lost := l;
        left := M.Crash.left ctl;
        let acked = List.rev !acked in
        (* A run with a fault that raises [Eio.Io] is judged by what it
           left. *)
        let out =
          match out with
          | Raised x when s.cfaults <> [] && is_io x -> Passed
          | o -> o
        in
        match (out, if out = Passed then settle () else None) with
        | Passed, (Some _ as e) -> e
        | Passed, None -> (
            let rpause = ref ignore in
            let rctl, rcut =
              let pause () = !rpause () in
              match s.rcut with
              | Some (j, st) ->
                  ( M.Crash.v ~os ~policy:(policy_of rtrail st) ~block_size:512
                      ~pause (),
                    j )
              | None -> (M.Crash.v ~os ~policy:Drop_unsynced ~pause (), None)
            in
            let rfs = M.Crash.wrap rctl base in
            recovering := true;
            Chaos_env.mark side "-- restart --";
            ack.alive <- (fun () -> not (M.Crash.is_crashed rctl));
            ack.on_ack <- ignore;
            let rout, _ =
              phase ~touch
                ~guard:(fun _ -> count_op halted ops)
                ~kept_going:kept_recover ~trace:rtrace ~ctl:rctl ~pause:rpause
                ~mono ~faults:[] ~cut:rcut ~cut_op:(ref None) ~seqs:(ref [])
                ~changed:rchanged ~stop rfs
                (fun fs cwd ->
                  in_env ~side ~ack clock mono ~fs ~cwd (recover ~acked))
            in
            match (rout, s.rcut) with
            | Passed, None -> None
            | Passed, Some _ -> (
                if not (M.Crash.is_crashed rctl) then M.Crash.crash rctl;
                ignore (M.Crash.post_crash rctl : Eio.Fs.dir_ty Eio.Path.t);
                ack.alive <- (fun () -> true);
                match settle () with
                | Some e -> Some e
                | None -> (
                    Chaos_env.mark side "-- restart --";
                    match
                      with_env ~side ~ack clock mono base (recover ~acked)
                    with
                    | () -> None
                    | exception x -> Some (Again x)))
            | o, _ -> Some (Recover o))
        | o, _ -> Some (Workload o)
      with
      | Ok e -> e
      | Error o ->
          if !kept_run then Some (Workload Kept_going)
          else if !kept_recover then Some (Recover Kept_going)
          else if !recovering then Some (Recover o)
          else Some (Workload o)
    in
    {
      trace;
      cut_op = !cut_op;
      seqs = !seqs;
      rtrace = kept rtrace;
      changed;
      rchanged;
      acked_at = List.rev !acked_at;
      lost = !lost;
      left = !left;
      trail = !trail;
      allowed =
        allows s.state !trail
        && Option.fold ~none:true
             ~some:(fun (_, st) -> allows st !rtrail)
             s.rcut;
      error;
    }

  let changed r (op : Op.t) = Hashtbl.mem r.changed op.index
  let rchanged r (op : Op.t) = Hashtbl.mem r.rchanged op.index

  (* The values acknowledged, by the operation each came after: their type
     has no printer, and recover names the one it misses. *)
  let acked_text = function
    | [] -> "none"
    | ats ->
        let n = List.length ats in
        let shown = List.take 8 ats in
        Fmt.str "%d value%s, after %s%s" n
          (if n = 1 then "" else "s")
          (String.concat ", "
             (List.map
                (function 0 -> "the start" | i -> Fmt.str "#%d" i)
                shown))
          (if n > 8 then Fmt.str " and %d more" (n - 8) else "")

  let state_text = function
    | Plan.Drop -> "every unsynced change lost"
    | Keep -> "every change kept, as after a process kill"
    | Enum _ as st ->
        Fmt.str "state %s of those the cut allows, tried one by one"
          (Plan.state_key st)
    | (Seed _ | Shape _) as st ->
        Fmt.str
          "state %s, drawn at random: the cut allows more states than its \
           share of the tier"
          (Plan.state_key st)

  let cut_text ?at = function
    | None -> "at the end"
    | Some p -> "before " ^ Plan.pos_text ?at p

  (* A change, numbered by the operation that made it, as the trace shows
     it. *)
  let change_text seqs (u : unsynced) =
    let n =
      match List.find_opt (fun (s, _) -> s < u.seq) seqs with
      | Some (_, i) -> Fmt.str "#%d " i
      | None -> ""
    in
    match u.kind with
    | Data -> Fmt.str "%swrite %s@%d+%d" n u.path u.file_offset u.length
    | Truncate -> Fmt.str "%struncate %s to %d" n u.path u.file_offset
    | Create -> Fmt.str "%screate %s" n u.path
    | Mkdir -> Fmt.str "%smkdir %s" n u.path
    | Unlink -> Fmt.str "%sunlink %s" n u.path
    | Rmdir -> Fmt.str "%srmdir %s" n u.path
    | Symlink { target } -> Fmt.str "%ssymlink %s -> %s" n u.path target
    | Rename { dest } -> Fmt.str "%srename %s to %s" n u.path dest
    | Chmod { perm } -> Fmt.str "%schmod %s %o" n u.path perm
    | Chown { uid; gid } -> Fmt.str "%schown %s %Ld:%Ld" n u.path uid gid

  let lines (s : Plan.crash) (r : run) =
    let at p =
      match r.cut_op with
      | Some op when Plan.at p op -> Some op
      | _ -> found r.trace p
    in
    [
      ("cut", Fmt.str "%s, %s" (cut_text ~at s.cut) (state_text s.state));
      ("faults", Plan.faults_text ~at s.cfaults);
      ( "recovery",
        match s.rcut with
        | None -> ""
        | Some (j, st) ->
            Fmt.str "cut in the restart %s, %s, then restart again" (cut_text j)
              (state_text st) );
      ("acked", acked_text r.acked_at);
      (* What the cut may have lost: all of it, none of it, or some. *)
      (let changes =
         String.concat "; " (List.map (change_text r.seqs) r.lost)
       in
       match s.state with
       | Drop -> ("lost", changes)
       | Keep -> ("lost", "")
       | Enum _ | Seed _ | Shape _ -> ("unsynced", changes));
      ( "left",
        match s.state with
        | Drop | Keep -> ""
        | Enum _ | Seed _ | Shape _ ->
            let n = List.length r.left in
            (List.take 8 r.left
            @ if n > 8 then [ Fmt.str "and %d more files" (n - 8) ] else [])
            |> String.concat "\n            " );
    ]

  (* The title of a failed run's report, and its error. *)
  let error_text = function
    | Workload o -> ("the workload failed", outcome_text o)
    | Settle (Lost _ as o) ->
        ( "an acknowledged file lost what it held: after the cut, before the \
           restart, it no longer holds what it held at its ack",
          outcome_text o )
    | Settle o -> ("inspect rejected what the cut left", outcome_text o)
    | Recover o ->
        ( "the restart failed: the workload, run again with no fault on what \
           the cut left, did not return",
          outcome_text o )
    | Again x ->
        ( "the restart failed: the workload, run again with no fault on what a \
           cut during the previous restart left, did not return",
          exn_text x )

  (* A state a cut allows, named by key [k] on top of a cut of the plan. *)
  let state_of_cut cuts k =
    match Plan.split_state_key k with
    | None -> None
    | Some (k', st) ->
        Option.map
          (fun (s : Plan.crash) -> { s with state = st })
          (List.find_opt (fun s -> Plan.crash_key s = k') cuts)

  (* A state drawn at random is one of a cut that allows more than two
     states, which the check may not try one by one. *)
  let drawn ~exec (s : Plan.crash) =
    let more (st : Plan.state) =
      Plan.next_choices (exec { s with state = st }).trail
    in
    match s.state with
    | Seed _ | Shape _ -> (
        match more (Enum []) with
        | None -> false
        | Some v -> Option.is_some (more (Enum v)))
    | Drop | Keep | Enum _ -> true

  (* The files [setup] makes, with their sizes. *)
  let setup_files ~name ~os ~setup =
    match
      in_loop (fun clock mono ->
          let d = fresh ~os clock in
          with_env ~side:(Chaos_env.side ()) clock mono d setup;
          files d "")
    with
    | Ok l -> l
    | Error o -> fail name "setup failed: %s" (outcome_text o)

  (* A shape is named by the choices its run made, which a replay makes
     again. *)
  let named (s : Plan.crash) (r : run) =
    match s.state with
    | Shape _ ->
        {
          s with
          state =
            Shape (List.rev_map (fun (c, n) -> if c < n then c else 0) r.trail);
        }
    | _ -> s

  let check_cuts ~name ?budget ?(seed = 0) ?replay ?os ?(setup = ignore)
      ?inspect run ~recover =
    let os = os_of os in
    let rng = Random.State.make [| seed |] in
    let budget = budget_of name budget in
    let replay =
      Option.map (parse_replay ~well_formed:Plan.well_formed name) replay
    in
    let exec ?cap ?budget s =
      exec ?cap ?budget ?inspect ~os ~setup run ~recover s
    in
    (* A replay runs one scenario, whatever the budget. *)
    let first =
      exec ?budget:(if replay = None then Some budget else None) Plan.base
    in
    (match first.error with
    | Some (Workload o) ->
        refuse_first name ~trace:first.trace o ~budget ~what:"cut"
    | _ -> ());
    let first_ops = kept first.trace in
    if first_ops = [] then
      fail name
        "the workload makes no filesystem operation, so it tests nothing";
    let n = List.length first_ops in
    let again = kept (exec ~cap:(n + 1) Plan.base).trace in
    (* A run keeps no more operations than four times the first run made:
       past that, the plan of the next scenarios reads its first ones. *)
    let exec = exec ~cap:((4 * (n + List.length first.rtrace)) + 10_000) in
    same_kinds name first_ops again;
    let hash = digest first_ops in
    let disk = Plan.Disk.v first_ops in
    let sizes = setup_files ~name ~os ~setup in
    (* Whether recover looked at the directory in some run. *)
    let reads = ref (first.rtrace <> []) in
    let tears = Plan.tears ~sizes first_ops in
    (* A run's result is dropped once the scenarios it leads to are drawn
       from it, so memory stays that of one run. *)
    let runs = ref 0 in
    let attempt ?matched s =
      incr runs;
      let r = exec s in
      let s = named s r in
      if r.rtrace <> [] then reads := true;
      (match r.error with
      | None -> ()
      | Some e ->
          let title, err = error_text e in
          report name ~title ~hash ~key:(Plan.crash_key s) ~matched
            ~trace:r.trace ~error:err (lines s r));
      r
    in
    let changed = changed first in
    let cuts = Plan.crash_cuts ~changed first_ops in
    let faults = Plan.crash_faults ~tears first_ops in
    match replay with
    | Some ((h, k) as r) -> (
        let matched = if h = hash then None else Some h in
        let find l = List.find_opt (fun s -> Plan.crash_key s = k) l in
        (* The scenarios each run of [l] leads to, one run at a time. *)
        let find_after l f = List.find_map (fun s -> find (f s (exec s))) l in
        let rcuts s r = Plan.rcuts ~changed:(rchanged r) s ~recover:r.rtrace in
        let candidates =
          [
            (fun () -> find (cuts @ faults));
            (fun () -> find_after cuts rcuts);
            (fun () -> find (Plan.crash_others ~changed ~tears first_ops));
            (fun () ->
              find_after faults (fun (s : Plan.crash) r ->
                  match s.cfaults with
                  | [ one ] when s.state = Drop ->
                      let changed, rest =
                        Plan.crash_seconds ~rng ~tears one ~first:disk
                          ~after:(kept r.trace)
                      in
                      changed @ rest
                  | _ -> []));
            (fun () -> find_after faults rcuts);
            (fun () -> state_of_cut cuts k);
          ]
        in
        let found = List.find_map (fun f -> f ()) candidates in
        (* A state drawn at random is drawn by a seed below a fourth of the
           budget. *)
        match found with
        | Some s when not (drawn ~exec s && (exec s).allowed) ->
            no_longer name ~hash r
        | Some { state = Seed n; _ } when n >= budget / 4 ->
            let need = 4 * (n + 1) in
            fail name
              "the replay names %s, a state drawn at random under a budget of \
               %d or more: pass ~budget:%d."
              k need need
        | Some s -> ignore (attempt ?matched s)
        | None -> no_longer name ~hash r)
    | None ->
        let over n =
          fail name
            "the linear tier needs %d runs, more than the budget of %d, and a \
             check never passes on a partial one. Shrink the workload, or \
             raise ~budget."
            n budget
        in
        let linear = List.length cuts + List.length faults in
        if linear > budget then over linear;
        (* Evictions are linear runs the sampled tier does not grow with, and
           no second fault or cut in the restart follows them. *)
        let evicted (s : Plan.crash) = List.exists Plan.is_evict s.cfaults in
        let evicts, faults = List.partition evicted faults in
        (* The second faults and the cuts during the restart each run of the
           linear tier allows, drawn as it ends, so no run's operations
           outlive it: an even share of what the sampled tier can spend. *)
        let total =
          Plan.sampled_share ~budget
            ~linear:(linear - List.length evicts)
            ~classes:1
        in
        let quota =
          let k = List.length cuts + List.length faults in
          (total + k - 1) / max 1 k
        in
        let best = Plan.Pairs.v total in
        let recovery =
          List.concat_map
            (fun (s : Plan.crash) ->
              let r = attempt s in
              (match s.cfaults with
              | [ one ] when s.state = Drop ->
                  Plan.Pairs.add best
                    (Plan.crash_seconds ~limit:total ~rng ~tears one ~first:disk
                       ~after:(kept r.trace))
              | _ -> ());
              Plan.sample rng quota
                (Plan.rcuts ~changed:(rchanged r) s ~recover:r.rtrace))
            (cuts @ faults)
        in
        List.iter (fun s -> ignore (attempt s)) evicts;
        (* Sampled tier. *)
        let thunks = List.map (fun s () -> ignore (attempt s)) in
        let others = thunks (Plan.crash_others ~changed ~tears first_ops) in
        let pairs = thunks (Plan.Pairs.to_list best) in
        let recovery = thunks recovery in
        (* Every state each cut allows, enumerated while the draws met so
           far allow no more than the cut's share, and drawn at random past
           that. Each cut gets an even share of what is left: first every cut
           whose states fit in it, then, with what those left, the others,
           smallest first. Of a cut past its share only its size is kept, so
           memory stays that of one cut: one that fits in its new share is
           enumerated again from its first state. *)
        let drops = List.filter (fun (s : Plan.crash) -> s.state = Drop) cuts in
        let enumerate (s : Plan.crash) tree spent allowance next =
          let rec go = function
            | None -> None
            | Some v
              when Plan.Draws.estimate tree > allowance || !spent >= allowance
              ->
                Some v
            | Some v ->
                incr spent;
                let r = attempt { s with state = Enum v } in
                Plan.Draws.add tree r.trail;
                go (Plan.next_choices r.trail)
          in
          go next
        in
        (* The states of cut [s] one by one, up to [allowance] runs: [None]
           once they are all tried, else the draws' estimate. *)
        let enumerate_from_first s spent allowance =
          let tree = Plan.Draws.empty () in
          incr spent;
          let r = attempt { s with state = Enum [] } in
          Plan.Draws.add tree r.trail;
          match
            enumerate s tree spent allowance (Plan.next_choices r.trail)
          with
          | None -> None
          | Some _ -> Some (Plan.Draws.estimate tree)
        in
        let states left =
          let rec first left acc = function
            | [] -> (left, List.rev acc)
            | (s : Plan.crash) :: rest as cuts ->
                let allowance = max 1 (left / List.length cuts) in
                let spent = ref 0 in
                let acc =
                  match enumerate_from_first s spent allowance with
                  | None -> acc
                  | Some estimate -> (s, estimate, !spent) :: acc
                in
                first (left - !spent) acc rest
          in
          let left, large = first left [] drops in
          let large =
            List.stable_sort (fun (_, a, _) (_, b, _) -> Int.compare a b) large
          in
          let rec share_out left = function
            | [] -> ()
            | (s, estimate, before) :: rest as cuts ->
                let share = max 0 (left / List.length cuts) in
                let spent = ref 0 in
                if share = 0 then ()
                else if estimate <= before + share then
                  ignore
                    (enumerate_from_first s spent (before + share) : int option)
                else begin
                  (* The first draw is a seed, which needs no widths: its
                      draws give them to the others. *)
                  incr spent;
                  let r = attempt { s with state = Plan.shape ~widths:[] 0 } in
                  let widths = List.rev_map snd r.trail in
                  for i = 1 to share - 1 do
                    incr spent;
                    ignore (attempt { s with state = Plan.shape ~widths i })
                  done
                end;
                share_out (left - !spent) rest
          in
          share_out left large
        in
        let classes =
          List.filter
            (fun (c, _) -> c <> [])
            [
              (others, Plan.sample rng);
              (pairs, List.take);
              (recovery, Plan.sample rng);
            ]
        in
        let before = !runs in
        let share = total / 2 / max 1 (List.length classes) in
        List.iter
          (fun (c, pick) -> List.iter (fun run -> run ()) (pick share c))
          classes;
        states (total - (!runs - before));
        if not !reads then
          fail name
            "recover makes no filesystem operation in any run, so it checks \
             nothing a cut leaves"
end

module Power = struct
  type change = Crash.Unsynced.t

  let pp_change = Crash.Unsynced.pp

  type policy = Lose_unsynced | Keep of (change -> bool)
  type t = Crash.t

  let v ?os = function
    | Lose_unsynced -> Crash.v ?os Drop_unsynced
    | Keep p -> Crash.v ?os (Survive p)

  let wrap = Crash.wrap
  let unsynced = Crash.unsynced
  let cut = Crash.cut
end

(* {1 The check} *)

let name = "Eio_fs_chaos.run"

let run ?os ?budget ?seed ?replay ?setup ?inspect app =
  Chaos_env.keeping_random @@ fun () ->
  let faults ?replay () =
    check_faults ~name ?budget ?seed ?replay ?os ?setup ?inspect app
  in
  let cuts ?replay () =
    Crash.check_cuts ~name ?budget ?seed ?replay ?os ?setup ?inspect
      (fun ~ack:_ env -> app env)
      ~recover:(fun ~acked:_ env -> app env)
  in
  match replay with
  | None ->
      faults ();
      cuts ()
  | Some r ->
      (* A cut's key is the only one of the shape [Plan.well_formed] reads. *)
      let _, key = parse_replay name r in
      if Plan.well_formed key then cuts ~replay:r () else faults ~replay:r ()

let ack env file = Ack.ack "Eio_fs_chaos.ack" env file
let acked env = Ack.acked "Eio_fs_chaos.acked" env

let device ?os ?user dir =
  let fs_os = os_of os in
  Chaos_env.keeping_random @@ fun () ->
  let run f =
    match
      in_loop ~effects:{ Quiet_mock.handle = Systhread.handle }
      @@ fun clock mono ->
      with_env ~side:(Chaos_env.side ()) clock mono (fresh ~os:fs_os clock) f
    with
    | Ok got -> Ok got
    | Error o -> Error (outcome_text o)
    | exception (Eio.Io _ as x) -> Error ("raised " ^ exn_text x)
  in
  Device.check ?os ?user ~run dir

module Private = struct
  let check_faults ?budget ?seed ?replay ?os ?setup run =
    Chaos_env.keeping_random @@ fun () ->
    check_faults ~name ?budget ?seed ?replay ?os ?setup run

  let check_cuts ?budget ?seed ?replay ?os ?setup run ~recover =
    Chaos_env.keeping_random @@ fun () ->
    Crash.check_cuts ~name ?budget ?seed ?replay ?os ?setup run ~recover

  let scenarios = Device.scenarios
  let eio_posix_differs = Device.eio_posix_differs

  module Crash = M.Crash
  module Law = M.Law
  module Internal = M.Internal
end
