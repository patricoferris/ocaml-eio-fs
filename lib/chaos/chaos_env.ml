open! Hermetic

type env =
  < fs : Eio.Fs.dir_ty Eio.Path.t
  ; cwd : Eio.Fs.dir_ty Eio.Path.t
  ; clock : float Eio.Time.clock_ty Eio.Resource.t
  ; mono_clock : Eio.Time.Mono.ty Eio.Resource.t
  ; secure_random : Eio.Flow.source_ty Eio.Resource.t
  ; stdin : Eio.Flow.source_ty Eio.Resource.t
  ; stdout : Eio.Flow.sink_ty Eio.Resource.t
  ; stderr : Eio.Flow.sink_ty Eio.Resource.t
  ; domain_mgr : Eio.Domain_manager.ty Eio.Resource.t
  ; process_mgr : [ `Generic ] Eio.Process.mgr_ty Eio.Resource.t
  ; debug : Eio.Debug.t
  ; backend_id : string >

(* What a report shows of a program's output: its tail. The buffer holds
   at most twice that. *)
let shown = 2048

type side = {
  mutable envs : int;
  out : Buffer.t;
  mutable dropped : int;
  mutable printed : bool; (* whether a program printed anything *)
}

let side () =
  { envs = 0; out = Buffer.create 256; dropped = 0; printed = false }

let add t s =
  Buffer.add_string t.out s;
  let n = Buffer.length t.out in
  if n > 2 * shown then begin
    let tail = Buffer.sub t.out (n - shown) shown in
    t.dropped <- t.dropped + n - shown;
    Buffer.clear t.out;
    Buffer.add_string t.out tail
  end

let mark t line = add t (line ^ "\n")

(* The tail of [s] from [i] on, from the first line that starts there, or
   else from the first character. *)
let from s i =
  let n = String.length s in
  let j =
    match String.index_from_opt s i '\n' with
    | Some j when j + 1 < n -> j + 1
    | _ ->
        let rec char j =
          if j >= n || Uchar.utf_decode_is_valid (String.get_utf_8_uchar s j)
          then j
          else char (j + 1)
        in
        char i
  in
  (j, String.sub s j (n - j))

let printed t =
  if not t.printed then ""
  else
    let s = Buffer.contents t.out in
    let n = String.length s in
    let cut, tail = if n > shown then from s (n - shown) else (0, s) in
    let dropped = t.dropped + cut in
    if dropped = 0 then tail else Fmt.str "... %d bytes before\n%s" dropped tail

type Eio.Exn.Backend.t += Refused of string

let () =
  Eio.Exn.Backend.register_pp (fun ppf -> function
    | Refused s ->
        Fmt.pf ppf "chaos:%s: a run of a check starts no process" s;
        true
    | _ -> false)

let refused what = Eio.Exn.create (Eio.Exn.X (Refused what))

module Stdin = struct
  type t = unit

  let read_methods = []
  let single_read () _ = raise End_of_file
end

module Output = struct
  type t = side

  let single_write t bufs =
    List.fold_left
      (fun n b ->
        t.printed <- true;
        add t (Cstruct.to_string b);
        n + Cstruct.length b)
      0 bufs

  let copy t ~src = Eio.Flow.Pi.simple_copy ~single_write t ~src
end

module Entropy = struct
  type t = Random.State.t

  let read_methods = []

  let single_read t buf =
    for i = 0 to Cstruct.length buf - 1 do
      Cstruct.set_uint8 buf i (Random.State.int t 256)
    done;
    Cstruct.length buf
end

module Domains = struct
  type t = unit

  let run () fn = fn ~cancelled:(fst (Eio.Promise.create ()))
  let run_raw () fn = fn ()
end

module Processes = struct
  type tag = [ `Generic ]
  type t = unit

  let environment () = [||]
  let getenv_opt () _ = None
  let pipe () ~sw:_ = raise (refused "pipe")

  let spawn () ~sw:_ ?cwd:_ ?stdin:_ ?stdout:_ ?stderr:_ ?env:_ ?executable:_
      args =
    raise (refused ("spawn " ^ String.concat " " args))
end

let stdin = Eio.Resource.T ((), Eio.Flow.Pi.source (module Stdin))
let domain_mgr = Eio.Resource.T ((), Eio.Domain_manager.Pi.mgr (module Domains))
let process_mgr = Eio.Resource.T ((), Eio.Process.Pi.mgr (module Processes))

(* The seed of the [n]th env of a scenario: of the default [Random] state,
   and of [secure_random]. *)
let seed n = [| 0x5eed; n |]

let v t clock mono ~(fs : _ Eio.Path.t) ~(cwd : _ Eio.Path.t) : env =
  let n = t.envs in
  t.envs <- n + 1;
  Random.full_init (seed n);
  let entropy =
    Eio.Resource.T
      ( Random.State.make (Array.append (seed n) [| 1 |]),
        Eio.Flow.Pi.source (module Entropy) )
  in
  let output = Eio.Resource.T (t, Eio.Flow.Pi.sink (module Output)) in
  object
    method fs = (fs :> Eio.Fs.dir_ty Eio.Path.t)
    method cwd = (cwd :> Eio.Fs.dir_ty Eio.Path.t)
    method clock = clock
    method mono_clock = mono
    method secure_random = entropy
    method stdin = stdin
    method stdout = output
    method stderr = output
    method domain_mgr = domain_mgr
    method process_mgr = process_mgr
    method debug = Eio.Private.Debug.v
    method backend_id = "chaos"
  end

let within t f =
  let traceln =
    {
      Eio.Debug.traceln =
        (fun ?__POS__:_ fmt ->
          Format.kasprintf
            (fun s ->
              t.printed <- true;
              mark t s)
            fmt);
    }
  in
  Eio.Fiber.with_binding Eio.Private.Debug.v#traceln traceln f

let keeping_random f =
  let saved = Random.get_state () in
  Fun.protect ~finally:(fun () -> Random.set_state saved) f
