(** The Eio backend the recorder runs the scenarios under. *)

val run : (Eio.Fs.dir_ty Eio.Path.t -> 'a) -> 'a
(** [run f] is [f cwd], with [cwd] the current directory as a sandbox, under the
    system's Eio backend: eio_windows on Windows, eio_posix elsewhere. *)
