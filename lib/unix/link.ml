open Lock

type t = { parent : Eio_unix.Fd.t; name : string; path : string }
type publication = Created | Present

let err_refused path reason =
  Error
    (`Invalid
       (Fmt.str
          "cannot publish symbolic link %s: %s; repair or remove the \
           conflicting entry before retrying"
          path reason))

let components path =
  if Eio_fs_path.is_abs path then Error "expected a relative path"
  else
    match Eio_fs_path.canonical ("/" ^ path) with
    | Ok parts -> Ok parts
    | Error _ -> Error "expected a non-empty canonical path without NUL"

(* [fn] applied to [dir]'s descriptor on a system thread, [dir] kept open
   meanwhile. *)
let at dir fn =
  Eio_unix.Fd.use_exn "Eio_fs_unix.Link" dir @@ fun dir ->
  Eio_unix.run_in_systhread ~label:"eio-fs-link" (fun () -> fn dir)

(* A missing parent is made as [create] says, and one another caller made
   meanwhile is opened all the same. *)
let rec open_parent ~create fd name =
  match at fd (fun dir -> At.open_dir dir name) with
  | opened -> opened
  | exception (Unix.Unix_error (Unix.ENOENT, _, _) as e) -> (
      match create with
      | `Never -> raise e
      | `If_missing perm ->
          (try at fd (fun dir -> At.mkdir dir name perm)
           with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
          open_parent ~create:`Never fd name)

let with_parent ?(create = `Never) (input : _ Eio.Path.t) fn =
  let dir, path = (input :> Eio.Fs.dir_ty Eio.Path.t) in
  let original = Eio_fs.Tree.name input in
  let components =
    match components path with
    | Ok components -> components
    | Error reason ->
        Fmt.kstr At.unsafe_parent "%s: %s; name a path beneath the directory"
          original reason
  in
  Eio.Switch.run @@ fun sw ->
  let rec descend fd = function
    | [] -> assert false
    | [ name ] -> { parent = fd; name; path = original }
    | name :: rest ->
        let next = open_parent ~create fd name in
        descend (Eio_unix.Fd.of_unix ~sw ~close_unix:true next) rest
  in
  let parent =
    try
      let directory = Eio.Path.open_dir ~sw (dir, ".") in
      let descriptor =
        match Eio_unix.Resource.fd_opt (Eio.Path.open_in ~sw directory) with
        | Some fd -> fd
        | None ->
            Fmt.invalid_arg
              "Eio_fs_unix.Link.with_parent: path is %s, in a directory with \
               no Unix descriptor"
              original
      in
      eio_unix_errors (fun () -> descend descriptor components)
    with Eio.Io _ as ex ->
      let bt = Printexc.get_raw_backtrace () in
      Eio.Exn.reraise_with_context ex bt "opening the parents of %s" original
  in
  fn parent

let publish t ~target =
  let existing () =
    let stat = at t.parent (fun dir -> At.lstat dir t.name) in
    match At.kind_of_mode (Int64.to_int stat.mode) with
    | `Symbolic_link ->
        let held = at t.parent (fun dir -> At.read_link dir t.name) in
        if String.equal held target then Ok Present
        else Error (`Changed (Some held))
    | `Regular_file -> err_refused t.path "regular file already exists"
    | `Directory -> err_refused t.path "directory already exists"
    | _ -> err_refused t.path "foreign filesystem entry already exists"
  in
  if String.equal target "" || String.contains target '\000' then
    err_refused t.path "target must be non-empty and contain no NUL"
  else
    match
      match at t.parent (fun dir -> At.symlink target dir t.name) with
      | () -> Ok Created
      | exception Unix.Unix_error (Unix.EEXIST, _, _) -> existing ()
    with
    | result -> result
    | exception Unix.Unix_error (code, operation, argument) ->
        err_refused t.path
          (Fmt.str "%s %s: %s" operation argument (Unix.error_message code))
    | exception (Eio.Io _ as ex) -> err_refused t.path (Printexc.to_string ex)

let rec read_into fd (buffer : Cstruct.t) =
  match Unix.read_bigarray fd buffer.buffer buffer.off buffer.len with
  | got -> got
  | exception Unix.Unix_error (Unix.EINTR, _, _) -> read_into fd buffer

(* The leaf is opened, weighed and read on system threads, one per call, so
   a cancel is seen between reads; it is closed before the result. *)
type regular = { contents : string; perm : Eio.File.Unix_perm.t }

let read_regular ~max_bytes t =
  if max_bytes < 0 || max_bytes = max_int then
    Fmt.invalid_arg
      "Eio_fs_unix.Link.read_regular: max_bytes is %d, must be between 0 and \
       max_int - 1"
      max_bytes;
  let err_read fmt = Fmt.kstr (fun msg -> Error (`Invalid msg)) fmt in
  let opened =
    match at t.parent (fun dir -> At.open_leaf dir t.name) with
    | fd -> Ok (Some fd)
    | exception Unix.Unix_error (Unix.ENOENT, _, _) -> Ok None
    | exception Unix.Unix_error (code, operation, argument) ->
        err_read "%s: %s %s: %s" t.path operation argument
          (Unix.error_message code)
    | exception (Eio.Io _ as ex) ->
        err_read "%s: %s" t.path (Printexc.to_string ex)
  in
  match opened with
  | Error _ as error -> error
  | Ok None -> Ok None
  | Ok (Some fd) -> (
      Fun.protect ~finally:(fun () ->
          try Unix.close fd with Unix.Unix_error _ -> ())
      @@ fun () ->
      let in_systhread fn =
        eio_unix_errors @@ fun () ->
        Eio_unix.run_in_systhread ~label:"eio-fs-link-read" fn
      in
      let stat = At.stat_of_raw (in_systhread (fun () -> At.fstat fd)) in
      let size = Optint.Int63.to_int64 stat.size in
      if stat.kind <> `Regular_file then
        err_read "%s is not a regular file" t.path
      else if size < 0L || size > Int64.of_int max_bytes then Error `Too_large
      else
        match
          Eio_fs_internal.bounded_read ~max_bytes:(Int64.to_int size)
            ~validate:(fun _ -> Ok ())
            ~stat
            ~read:(fun buffer -> in_systhread (fun () -> read_into fd buffer))
        with
        | Ok bytes ->
            Ok (Some { contents = Cstruct.to_string bytes; perm = stat.perm })
        | Error `Too_large -> Error `Too_large
        | Error (`Invalid reason) -> err_read "%s: %s" t.path reason)
