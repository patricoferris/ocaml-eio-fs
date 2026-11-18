let file f =
  match Eio_fs.Sync.file_barrier f with
  | Some barrier -> barrier ~full:true
  | None -> invalid_arg "Full.file: no registered barrier"

let dir ((res, path) : _ Eio.Path.t) =
  match Eio_fs.Sync.dir_barrier res with
  | Some barrier -> barrier ~path ~full:true
  | None -> invalid_arg "Full.dir: no registered barrier"
