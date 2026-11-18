(** A toy remote filesystem, over [env#net]: each operation of its directories
    and files runs on a server's directory, a subdirectory of [env#fs] that each
    one gets. A request crosses one connection, to a server on the same network,
    as its number, and the server answers with the number once it ran the
    request; the operation itself is shared memory, as both ends live in one
    process. *)

val v :
  ?leak:bool -> sw:Eio.Switch.t -> Eio_chaos.env -> Eio.Fs.dir_ty Eio.Path.t
(** [v ~sw env] is a new remote filesystem, its server and its client on [sw]. A
    network error, or no answer in 5 seconds, is an input/output error, as NFS
    gives one; with [leak] (defaults to [false]), the network's own error
    reaches the caller. *)
