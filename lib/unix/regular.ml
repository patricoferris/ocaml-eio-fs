open Lock

(* [fn] applied to [dir]'s descriptor on a system thread, [dir] kept open
   meanwhile. *)
let at dir fn =
  Eio_unix.Fd.use_exn "Eio_fs_unix.read_regular" dir @@ fun dir ->
  Eio_unix.run_in_systhread ~label:"eio-fs-read-regular" (fun () -> fn dir)

(* The directory holding [path], as a host descriptor, and [path]'s last
   component in it. *)
let with_parent_fd path fn =
  let parent, name =
    match Eio.Path.split path with
    | Some pair -> pair
    | None ->
        Fmt.invalid_arg
          "Eio_fs_unix.read_regular: path is %a, the root of its capability, \
           which has no parent"
          Eio.Path.pp path
  in
  Eio.Switch.run @@ fun sw ->
  let directory = Eio.Path.open_dir ~sw parent in
  match Eio_unix.Resource.fd_opt (Eio.Path.open_in ~sw directory) with
  | Some fd -> fn fd name
  | None ->
      Fmt.invalid_arg
        "Eio_fs_unix.read_regular: path is %s, in a directory with no Unix \
         descriptor"
        (Eio_fs_internal.native_name path)

let rec read_into fd (buffer : Cstruct.t) =
  match Unix.read_bigarray fd buffer.buffer buffer.off buffer.len with
  | got -> got
  | exception Unix.Unix_error (Unix.EINTR, _, _) -> read_into fd buffer

(* The leaf is opened, weighed and read on system threads, one per call, so
   a cancel is seen between reads; it is closed before the result. *)
type t = { contents : string; perm : Eio.File.Unix_perm.t }

let pp ppf t = Fmt.pf ppf "%d bytes, mode %o" (String.length t.contents) t.perm

let read ~max_bytes path =
  if max_bytes < 0 || max_bytes = max_int then
    Fmt.invalid_arg
      "Eio_fs_unix.read_regular: max_bytes is %d, must be between 0 and \
       max_int - 1"
      max_bytes;
  (* A leaf of [Eio_fs.Nofollow.with_parent] is spelled through its held
     directory's dot entry. *)
  let shown = Eio_fs_path.normalize (Eio_fs_internal.native_name path) in
  let err_read fmt = Fmt.kstr (fun msg -> Error (`Invalid msg)) fmt in
  with_parent_fd path @@ fun parent name ->
  let opened =
    match at parent (fun dir -> At.open_leaf dir name) with
    | fd -> Ok (Some fd)
    | exception Unix.Unix_error (Unix.ENOENT, _, _) -> Ok None
    | exception Unix.Unix_error (code, operation, argument) ->
        err_read "%s: %s %s: %s" shown operation argument
          (Unix.error_message code)
    | exception (Eio.Io _ as ex) ->
        err_read "%s: %s" shown (Printexc.to_string ex)
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
        Eio_unix.run_in_systhread ~label:"eio-fs-read-regular" fn
      in
      let stat = At.stat_of_raw (in_systhread (fun () -> At.fstat fd)) in
      let size = Optint.Int63.to_int64 stat.size in
      if stat.kind <> `Regular_file then
        err_read "%s is not a regular file" shown
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
        | Error (`Invalid reason) -> err_read "%s: %s" shown reason)
