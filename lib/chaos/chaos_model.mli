(** The modules this package's tests reach through [Eio_fs_chaos.Private]. *)

module Crash = Crash

(* It hands out nothing a schedule's holder could not reach by running an
   operation through a wrapper. *)
module Law = Law
(** What {!Wrapper.wrap} does with each operation of each interface it binds. *)

module Internal : sig
  val crash_image :
    Crash.t ->
    (string * int64 * [ `File of string | `Dir | `Link of string ]) list
  (** [crash_image t] is {!Crash.image}. *)

  val descriptors : _ Eio.Path.t -> int option
  (** [descriptors path] is how many descriptors are open on the in-memory
      filesystem of [path], or [None] if [path] is not in one. *)

  val replay_copied : unit -> int
  (** [replay_copied ()] is how many bytes {!Crash} has copied into the images
      it replays a file's changes over, since the program started. *)
end
