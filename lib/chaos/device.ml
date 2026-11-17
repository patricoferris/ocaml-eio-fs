open! Hermetic
module K = Eio_fs_kernel

type os = [ `Linux | `Macos ]

let os_name : os -> string = function `Linux -> "Linux" | `Macos -> "macOS"

type row = {
  linux : string list;
  macos : string list;
  linux_eio : string list;
  macos_eio : string list;
}

let rows =
  lazy
    (String.split_on_char '\n' Device_traces.csv
    |> List.filter (fun l -> l <> "" && not (String.starts_with ~prefix:"#" l))
    |> List.tl
    |> List.map (fun line ->
        let outcomes s = String.split_on_char '|' s in
        match String.split_on_char ',' line with
        | [ ops; l; m; le; me ] ->
            ( ops,
              {
                linux = outcomes l;
                macos = outcomes m;
                linux_eio = outcomes le;
                macos_eio = outcomes me;
              } )
        | _ -> Fmt.failwith "Eio_fs: a malformed kernel trace line: %s" line))

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
  "the recorder asks the kernel through Unix, which cannot make an earlier \
   operation, so from there the answer is eio_posix's"

type answer = { want : string; from : string; why : string option }

(* Each operation's answer: the kernel's, or eio_posix's where a rule says
   so and from an operation [Unix] cannot make on. *)
let answers (os : os) name ops =
  let row =
    match List.assoc_opt (K.key ops) (Lazy.force rows) with
    | Some r -> r
    | None -> Fmt.failwith "Eio_fs: no kernel trace for the scenario %S" name
  in
  let kernel, eio =
    match os with
    | `Linux -> (row.linux, row.linux_eio)
    | `Macos -> (row.macos, row.macos_eio)
  in
  let cut =
    let rec go j = function
      | [] -> max_int
      | "-" :: _ -> j
      | _ :: l -> go (j + 1) l
    in
    go 0 kernel
  in
  let posix = "eio_posix on " ^ os_name os in
  List.mapi
    (fun j _ ->
      let ruled =
        List.find_opt
          (fun r -> r.os = os && r.scenario = name && List.mem (j + 1) r.ops)
          rules
      in
      match ruled with
      | Some r -> { want = List.nth eio j; from = posix; why = Some r.why }
      | None when j >= cut ->
          { want = List.nth eio j; from = posix; why = Some unix_cannot }
      | None -> { want = List.nth kernel j; from = os_name os; why = None })
    ops

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

let ids (user : Eio_fs.user) =
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

(* A scenario that differs: its operations up to the first answer outside
   what [os] gave, that answer, and the one expected. *)
let differs ~scenario ~ops ~got j (a : answer) =
  let upto = List.filteri (fun i _ -> i <= j) (List.map String.escaped ops) in
  let line =
    Fmt.str "  %s: %s: expected one of {%s}, got %s" scenario
      (String.concat "; " upto) a.want (List.nth got j)
  in
  match a.why with
  | None -> line
  | Some why -> Fmt.str "%s@\n    (%s: %s)" line a.from why

let check ?os ?user f =
  let os : os =
    match os with Some `Macos -> `Macos | Some `Linux | None -> `Linux
  in
  let ids, run =
    match user with
    | Some u -> (ids u, fun _ -> true)
    | None ->
        ({ K.gid = 0; other_gid = 0 }, fun s -> not (List.mem s needs_user))
  in
  let differed =
    List.concat
      (List.mapi
         (fun i (scenario, ops) ->
           if not (run scenario) then []
           else
             let answers = answers os scenario ops in
             let got =
               Eio.Switch.run @@ fun sw ->
               let dir = Eio.Path.(f ~sw / string_of_int i) in
               Eio.Path.mkdir ~perm:0o755 dir;
               K.run ids dir ops
             in
             let rec first j = function
               | [] -> []
               | (a : answer) :: rest ->
                   if List.nth got j <> a.want then
                     [ differs ~scenario ~ops ~got j a ]
                   else first (j + 1) rest
             in
             first 0 answers)
         K.scenarios)
  in
  if differed <> [] then begin
    let left = if user = None then List.length needs_user else 0 in
    let failed = List.length differed in
    let ran = List.length K.scenarios - left in
    Fmt.failwith
      "%s: %d of %d scenarios differ from %s@\n\
       %s@\n\
      \  %d passed, %d differed, %d left out%s"
      name failed ran (os_name os)
      (String.concat "\n" differed)
      (ran - failed) failed left
      (if left > 0 then Fmt.str "; %d that need ~user" left else "")
  end

let eio_posix_differs os =
  let os : os = match os with `Macos -> `Macos | `Linux -> `Linux in
  List.filter_map
    (fun (scenario, ops) ->
      let row = List.assoc (K.key ops) (Lazy.force rows) in
      let eio =
        match os with `Linux -> row.linux_eio | `Macos -> row.macos_eio
      in
      let want = List.map (fun a -> a.want) (answers os scenario ops) in
      if eio <> want then Some scenario else None)
    K.scenarios
