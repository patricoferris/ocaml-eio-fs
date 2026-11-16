(** What the cases of this executable share: a fresh directory, another process
    holding a lock, and how a process ended. *)

val with_real_dir_env :
  (Eio_unix.Stdenv.base -> Eio.Fs.dir_ty Eio.Path.t -> 'a) -> 'a
(** [with_real_dir_env fn] is [fn env dir], [dir] a fresh directory of the real
    file system, removed once [fn] returns. *)

val with_real_dir : (Eio.Fs.dir_ty Eio.Path.t -> 'a) -> 'a
(** [with_real_dir fn] is {!with_real_dir_env} without the environment. *)

val with_in_memory_fs : (Eio.Fs.dir_ty Eio.Path.t -> 'a) -> 'a
(** [with_in_memory_fs fn] is [fn root], [root] an in-memory file system, under
    Eio's mock backend. *)

val mentions : needle:string -> string -> bool
(** [mentions ~needle s] is [true] iff [needle] occurs in [s]. *)

val holding_process :
  sw:Eio.Switch.t ->
  env:Eio_unix.Stdenv.base ->
  kind:string ->
  lock:_ Eio.Path.t ->
  Eio_unix.Process.ty Eio.Resource.t
  * (unit -> [ `Exited of int | `Signaled of int ])
(** [holding_process ~sw ~env ~kind ~lock] is another process holding [lock] by
    [kind], ["record"] or ["lease"] ([hold_until_release] in [test.ml]), once it
    said it holds it, and the function that tells it to let go and answers how
    it ended: [`Exited 0] when it let go then, [`Exited 3] when it let go at its
    own five-second bound. *)

val status : [ `Exited of int | `Signaled of int ] Alcotest.testable
(** [status] compares how a process ended. *)

val refused : (unit -> unit) -> bool
(** [refused f] is [true] iff [f ()] raises [Eio_fs_unix.Lock_unavailable]. *)

val reap : int -> int * Unix.process_status
(** [reap pid] waits for the process [pid], again after [EINTR]. *)

val exited : Unix.process_status -> int -> bool
(** [exited status code] is [true] iff [status] is an exit with [code]. *)

val probe : string -> _ Eio.Path.t -> int -> unit
(** [probe mode path code] runs this executable as another process that tries
    [mode] on [path], and checks it exits with [code]: 3 when a holder refuses
    it, 0 when it is granted. *)

val owner_arg : string
(** [owner_arg] is the argument that runs {!shared_lease_outlives_its_owner} as
    a process of its own. *)

val shared_lease_outlives_its_owner : unit -> unit
(** [shared_lease_outlives_its_owner ()] checks that a shared lease an owner's
    child inherited holds after the owner exits, until the child closes it. It
    forks, so it runs before any domain is spawned. *)
