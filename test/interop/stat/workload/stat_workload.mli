(** Workloads whose stat answers, times and link and block counts, are recorded
    on Linux through eio_posix and replayed on the in-memory filesystem. *)

type t = tick:(unit -> unit) -> Eio.Fs.dir_ty Eio.Path.t -> string list
(** The type for a workload. [w ~tick dir] runs it in the empty directory [dir],
    calling [tick ()] wherever time must pass, and is its answer, one line per
    step. *)

val all : (string * t) list
(** [all] is each workload's name and the workload. *)
