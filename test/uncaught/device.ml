(* A device check that fails, its report left uncaught. *)

let user =
  { Eio_fs_mem.uid = 1000L; gid = 1000L; groups = [ 4242L ]; umask = 0o022 }

let no_stat_of_a (op : Eio_fs_chaos.Op.t) =
  if op.kind = `Stat && Filename.basename op.path = "A" then
    Some (Eio_fs_chaos.Fault.Fail `Enoent)
  else None

let () =
  Eio_fs_chaos.device ~os:`Macos ~user (fun ~sw:_ _ ->
      Eio_fs_chaos.wrap no_stat_of_a (Eio_fs_mem.v ~os:`Macos ~user ()))
