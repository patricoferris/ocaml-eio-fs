(** The files a run acknowledged, each with what it held then, read beneath the
    fault wrapper. A file the run writes, truncates, renames or removes since is
    no longer compared. *)

type entry
(** The type for an acknowledged file with what it held. *)

type t = {
  base : Eio.Fs.dir_ty Eio.Path.t;
  at : unit -> int;  (** The last operation's index. *)
  mutable alive : unit -> bool;  (** [false] once the power is cut. *)
  mutable on_ack : unit -> unit;  (** Runs after each acknowledgement. *)
  mutable live : entry list;  (** Latest first. *)
  mutable paths : string list;  (** Latest first, each once. *)
}

val v : at:(unit -> int) -> Eio.Fs.dir_ty Eio.Path.t -> t
(** [v ~at base] has acknowledged no file of [base]. *)

val ack : string -> t -> _ Eio.Path.t -> unit
(** [ack name t file] records what [file] holds now, unless the power is cut.

    @raise Invalid_argument
      naming [name] if [file] is not a regular file of [t]'s filesystem. *)

val acked : t -> string list
(** [acked t] is the paths acknowledged, in the order of their first
    acknowledgement, each from the root. *)

val touch : t -> Op.t -> unit
(** [touch t op] stops comparing the files [op] changes. *)

val lost : t -> string option
(** [lost t] says the first file acknowledged and untouched since that no longer
    holds what it held then. *)
