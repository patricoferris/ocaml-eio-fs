open! Hermetic
module K = Eio_fs_kernel

type os = [ `Linux | `Macos | `Windows ]

let os_name : os -> string = function
  | `Linux -> "Linux"
  | `Macos -> "macOS"
  | `Windows -> "Windows"

(* A trace: each row's outcomes by the name of their column, keyed by the
   row's operations. A column is found by its name, so one added later
   changes nothing here. *)
let parse csv =
  let lines =
    String.split_on_char '\n' csv
    |> List.filter (fun l -> l <> "" && not (String.starts_with ~prefix:"#" l))
  in
  match lines with
  | [] -> Fmt.failwith "Eio_fs: an empty kernel trace"
  | header :: lines ->
      let columns = List.tl (String.split_on_char ',' header) in
      List.map
        (fun line ->
          match String.split_on_char ',' line with
          | ops :: cols when List.length cols = List.length columns ->
              ( ops,
                List.combine columns (List.map (String.split_on_char '|') cols)
              )
          | _ -> Fmt.failwith "Eio_fs: a malformed kernel trace line: %s" line)
        lines

let shipped = lazy (parse Device_traces.csv)

(* The columns that hold [os]'s answers: the kernel's straight, where the
   recorder could reach it, and Eio's backend's. On Windows only
   eio_windows's were recorded, as an administrator and as a basic user. *)
let columns ~basic : os -> string option * string = function
  | `Linux -> (Some "linux", "linux eio_posix")
  | `Macos -> (Some "macos", "macos eio_posix")
  | `Windows when basic -> (None, "windows basic user eio_windows")
  | `Windows -> (None, "windows eio_windows")

let column row name =
  match List.assoc_opt name row with
  | Some c -> c
  | None -> Fmt.failwith "Eio_fs: the kernel trace has no column %S" name

(* Operations where an Eio directory answers as eio_posix does, not as the
   kernel, and why. *)
type rule = { os : os; scenario : string; ops : int list; why : string }

let path_max =
  "eio_posix resolves a path one component at a time, so no path is too long"

let rules =
  [
    { os = `Linux; scenario = "path max"; ops = [ 2 ]; why = path_max };
    { os = `Macos; scenario = "path max"; ops = [ 1; 2; 3 ]; why = path_max };
    {
      os = `Linux;
      scenario = "dir search only";
      ops = [ 4 ];
      why = "eio_posix opens a directory O_PATH, which needs no read bit";
    };
  ]

let unix_cannot =
  "the recorder could not make an earlier operation of this scenario through \
   Unix, so the kernel's own answer is unknown"

type answer = { want : string; from : string; why : string option }

(* Each operation's answer: the kernel's, or the backend's where a rule says
   so, from an operation [Unix] cannot make on, and on Windows. [None] when
   the trace did not record the scenario there. *)
let answers ?(trace = shipped) ~basic (os : os) name ops =
  let row =
    match List.assoc_opt (K.key ops) (Lazy.force trace) with
    | Some r -> r
    | None -> Fmt.failwith "Eio_fs: no kernel trace for the scenario %S" name
  in
  let kernel_column, eio_column = columns ~basic os in
  let eio = column row eio_column in
  let kernel = Option.fold ~none:eio ~some:(column row) kernel_column in
  if eio = [ "not recorded" ] || kernel = [ "not recorded" ] then None
  else
    let cut =
      let rec go j = function
        | [] -> max_int
        | "-" :: _ -> j
        | _ :: l -> go (j + 1) l
      in
      go 0 kernel
    in
    let backend =
      Fmt.str "%s on %s"
        (if os = `Windows then "eio_windows" else "eio_posix")
        (os_name os)
    in
    Some
      (List.mapi
         (fun j _ ->
           let ruled =
             List.find_opt
               (fun r ->
                 r.os = os && r.scenario = name && List.mem (j + 1) r.ops)
               rules
           in
           match ruled with
           | Some r ->
               { want = List.nth eio j; from = backend; why = Some r.why }
           | None when j >= cut ->
               { want = List.nth eio j; from = backend; why = Some unix_cannot }
           | None -> { want = List.nth kernel j; from = os_name os; why = None })
         ops)

let expected ?trace ?(basic = false) os name =
  let os = (os :> os) in
  let trace = Option.map (fun csv -> lazy (parse csv)) trace in
  match List.assoc_opt name K.scenarios with
  | None -> Fmt.invalid_arg "Eio_fs: no scenario %S" name
  | Some ops ->
      Option.map
        (List.map (fun a -> a.want))
        (answers ?trace ~basic os name ops)

let scenarios = List.map fst K.scenarios
let name = "Eio_fs_chaos.device"

(* The answers of these were a user's: root passes every permission check,
   and the others change a file's group to a supplementary one. *)
let needs_user =
  [
    "dir no write";
    "dir write and search";
    "dir no read";
    "dir no search";
    "dir search only";
    "file no read";
    "file no write";
    "group inherited";
    "setgid dir";
    "write clears setuid";
    "chgrp";
  ]

let ids (user : Eio_fs_mem.user) =
  if Int64.equal user.uid 0L then
    Fmt.invalid_arg
      "%s: the user is root, who needs no permission bit; the answers are a \
       user's"
      name;
  if user.umask <> 0o022 then
    Fmt.invalid_arg "%s: the umask is %03o; the answers are umask 022's" name
      user.umask;
  match List.find_opt (fun g -> not (Int64.equal g user.gid)) user.groups with
  | None ->
      Fmt.invalid_arg
        "%s: the user has no supplementary group; the answers change a file's \
         group to one"
        name
  | Some g -> { K.gid = Int64.to_int user.gid; other_gid = Int64.to_int g }

(* An answer on one line: an error Eio prints on several is joined. *)
let one_line s =
  String.split_on_char '\n' s |> List.map String.trim |> String.concat " "

(* A scenario that differs: its operations up to the first answer outside
   what [os] gave, that answer, and the one expected, marked when eio_posix
   differs there too. *)
let differs ~posix ~scenario ~ops ~got j (a : answer) =
  let upto = List.filteri (fun i _ -> i <= j) (List.map String.escaped ops) in
  let line =
    Fmt.str "  %s: %s: expected %s, got %s%s" scenario (String.concat "; " upto)
      a.want
      (one_line (List.nth got j))
      (if posix then " (eio_posix differs here too)" else "")
  in
  match a.why with
  | None -> line
  | Some why -> Fmt.str "%s@\n    (expected as %s answers: %s)" line a.from why

let eio_posix_differs os =
  let os = (os :> os) in
  if os = `Windows then []
  else
    List.filter_map
      (fun (scenario, ops) ->
        let row = List.assoc (K.key ops) (Lazy.force shipped) in
        let eio = column row (snd (columns ~basic:false os)) in
        match answers ~basic:false os scenario ops with
        | Some answers when eio <> List.map (fun a -> a.want) answers ->
            Some scenario
        | Some _ | None -> None)
      K.scenarios

(* A word [@p] of a scenario names [p] under the scenario's directory by its
   absolute path, which only its native spelling gives. *)
let needs_native ops =
  List.exists
    (fun op ->
      List.exists
        (fun w -> String.starts_with ~prefix:"@" w)
        (String.split_on_char ' ' op))
    ops

(* Whether a native path is absolute, on POSIX or on Windows. *)
let absolute p =
  String.starts_with ~prefix:"/" p
  || String.starts_with ~prefix:"\\\\" p
  || (String.length p >= 3 && p.[1] = ':' && (p.[2] = '\\' || p.[2] = '/'))

type plan =
  | Run of { ids : K.ids; answers : answer list }
  | Left of [ `User | `Native | `Unrecorded ]

let os_of = function Some (#os as os) -> os | None -> `Linux

(* What [device] does with each scenario: run it, with the answers it
   expects, or leave it out, and why. Without [user] the scenarios that need
   one are left out; on Windows, the answers are a basic user's with it and an
   administrator's without. *)
let plan ?trace os user ~native =
  let ids, basic =
    match user with Some u -> (Some (ids u), true) | None -> (None, false)
  in
  List.map
    (fun (scenario, ops) ->
      let p =
        if ids = None && List.mem scenario needs_user then Left `User
        else if needs_native ops && not native then Left `Native
        else
          match answers ?trace ~basic os scenario ops with
          | None -> Left `Unrecorded
          | Some answers ->
              let ids =
                Option.value ~default:{ K.gid = 0; other_gid = 0 } ids
              in
              Run { ids; answers }
      in
      (scenario, ops, p))
    K.scenarios

(* The directory a scenario runs in, under [dir] opened as a sandbox, as the
   recorder ran it under its current directory, and that directory's native
   absolute path. *)
let scenario_dir ~sw dir name =
  let base = Eio.Path.open_dir ~sw dir in
  let d = Eio.Path.(base / name) in
  Eio.Path.mkdir ~perm:0o755 d;
  let root =
    match Eio.Path.native d with
    | Some p when absolute p -> Some p
    | Some _ | None -> None
  in
  (d, root)

(* Whether [dir]'s scenario directories have an absolute native path. *)
let has_native ~run f =
  match
    run (fun env ->
        Eio.Switch.run @@ fun sw ->
        let _, root = scenario_dir ~sw (f ~sw env) "native" in
        [ (if root = None then "no" else "yes") ])
  with
  | Ok [ "yes" ] -> true
  | Ok _ | Error _ -> false

let check ?os ?user ~run:run_in f =
  let os = os_of os in
  let posix = eio_posix_differs os in
  let native = has_native ~run:run_in f in
  let plan = plan os user ~native in
  let differed =
    List.concat
      (List.mapi
         (fun i (scenario, ops, p) ->
           match p with
           | Left _ -> []
           | Run { ids; answers } -> (
               let got =
                 run_in @@ fun env ->
                 Eio.Switch.run @@ fun sw ->
                 let dir, root =
                   scenario_dir ~sw (f ~sw env) (string_of_int i)
                 in
                 K.run ?root ids dir ops
               in
               let rec first got j = function
                 | [] -> []
                 | (a : answer) :: rest ->
                     if List.nth got j <> a.want then
                       [
                         differs ~posix:(List.mem scenario posix) ~scenario ~ops
                           ~got j a;
                       ]
                     else first got (j + 1) rest
               in
               match got with
               | Ok got -> first got 0 answers
               | Error e -> [ Fmt.str "  %s: %s" scenario (one_line e) ]))
         plan)
  in
  if differed <> [] then begin
    let count why =
      List.length (List.filter (fun (_, _, p) -> p = Left why) plan)
    in
    let reasons =
      List.filter_map
        (fun (why, n) -> if n > 0 then Some (Fmt.str "; %d %s" n why) else None)
        [
          ("that need ~user", count `User);
          ("that need a native path", count `Native);
          ("not recorded", count `Unrecorded);
        ]
    in
    let left = count `User + count `Native + count `Unrecorded in
    let failed = List.length differed in
    let ran = List.length K.scenarios - left in
    Fmt.failwith
      "%s: %d of %d scenarios differ from %s@\n\
       %s@\n\
      \  %d passed, %d differed, %d left out%s"
      name failed ran (os_name os)
      (String.concat "\n" differed)
      (ran - failed) failed left (String.concat "" reasons)
  end

(* An answer a failure below a device may explain: an error eio-fs injects,
   as it answers it, a missing file, a refused access, a read-only or full
   filesystem, or an input/output error. *)
let contains ~sub s =
  let n = String.length sub and m = String.length s in
  let rec go i = i + n <= m && (String.sub s i n = sub || go (i + 1)) in
  go 0

let injected a =
  a = "ENOENT" || a = "denied"
  || String.starts_with ~prefix:"exn Eio.Io" a
     &&
     let a = String.lowercase_ascii a in
     List.exists
       (fun sub -> contains ~sub a)
       [
         "eio ";
         "enospc";
         "erofs";
         "input/output error";
         "no space left on device";
         "read-only file system";
       ]

let program ?os ?user f (env : Eio_chaos.env) =
  let os = os_of os in
  let posix = eio_posix_differs os in
  let native =
    Eio.Switch.run @@ fun sw ->
    snd (scenario_dir ~sw (f ~sw env) "native") <> None
  in
  (* A restart replays every scenario in directories of its own. *)
  let suffix = if Eio_chaos.Private.restarted env then "r" else "" in
  List.iteri
    (fun i (scenario, ops, p) ->
      match p with
      | Left _ -> ()
      | Run { ids; answers } -> (
          let failed_before = Eio_chaos.faulted env#chaos in
          match
            Eio.Switch.run @@ fun sw ->
            let dir, root =
              scenario_dir ~sw (f ~sw env) (Fmt.str "%d%s" i suffix)
            in
            K.run ?root ids dir ops
          with
          | exception Eio.Io _ when failed_before || Eio_chaos.faulted env#chaos
            ->
              ()
          | got ->
              let lenient = Eio_chaos.faulted env#chaos in
              let rec check j = function
                | [] -> ()
                | (a : answer) :: rest ->
                    let g = List.nth got j in
                    if g = a.want then check (j + 1) rest
                    else if lenient && injected g then ()
                    else
                      failwith
                        (String.trim
                           (differs ~posix:(List.mem scenario posix) ~scenario
                              ~ops ~got j a))
              in
              check 0 answers))
    (plan os user ~native)
