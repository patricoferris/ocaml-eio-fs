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
    (String.split_on_char '\n' Conformance_traces.csv
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
let name = "Eio_fs.Conformance.check"

let ids (user : Low_level.User.t) =
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

let report ~os ~scenario ~ops ~got j (a : answer) =
  let op = String.escaped (List.nth ops j) in
  let lines =
    [
      ("scenario", scenario);
      ("operation", Fmt.str "%d, %s" (j + 1) op);
      (a.from, a.want);
      ("directory", List.nth got j);
    ]
    @ Option.fold ~none:[] ~some:(fun w -> [ ("why", w) ]) a.why
  in
  let upto =
    List.filteri
      (fun i _ -> i <= j)
      (List.combine (List.map String.escaped ops) got)
  in
  let width =
    List.fold_left (fun n (op, _) -> max n (String.length op)) 0 upto
  in
  let trace =
    upto
    |> List.mapi (fun i (op, got) ->
        Fmt.str "    %d %-*s  %s" (i + 1) width op got)
  in
  let line (k, v) = Fmt.str "  %-10s %s" (k ^ ":") v in
  Fmt.failwith
    "%s: %S differs from %s at operation %d@\n\
     %s@\n\
    \  trace:@\n\
     %s@\n\
    \  replay:    ~only:[%S]"
    name scenario (os_name os) (j + 1)
    (String.concat "\n" (List.map line lines))
    (String.concat "\n" trace) scenario

let check ?os ?only ~user f =
  let os : os =
    match os with Some `Macos -> `Macos | Some `Linux | None -> `Linux
  in
  let ids = ids user in
  let selected =
    match only with
    | None -> fun _ -> true
    | Some l ->
        List.iter
          (fun s ->
            if not (List.mem s scenarios) then
              Fmt.invalid_arg "%s: no scenario is named %S" name s)
          l;
        fun s -> List.mem s l
  in
  List.iteri
    (fun i (scenario, ops) ->
      if selected scenario then begin
        let answers = answers os scenario ops in
        let dir = Eio.Path.(f () / string_of_int i) in
        Eio.Path.mkdir ~perm:0o755 dir;
        let got = K.run ids dir ops in
        List.iteri
          (fun j (a : answer) ->
            if List.nth got j <> a.want then report ~os ~scenario ~ops ~got j a)
          answers
      end)
    K.scenarios

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
