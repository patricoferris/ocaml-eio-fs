(** The files a run acknowledged, each with what it held then, read beneath the
    fault wrapper. A file the run writes, truncates, renames or removes since is
    no longer compared. *)

type entry
(** The type for an acknowledged file with what it held. *)

type t
(** The type for the files a run acknowledged. *)

val v :
  at:(unit -> int) -> alive:(unit -> bool) -> Eio.Fs.dir_ty Eio.Path.t -> t
(** [v ~at ~alive base] has acknowledged no file of [base]. [at ()] is the index
    of the last operation, and [alive ()] is [false] once the power is cut. *)

val on_ack : t -> (unit -> unit) -> unit
(** [on_ack t f] runs [f] after each later acknowledgement, in place of what ran
    before. *)

val add : string -> t -> _ Eio.Path.t -> unit
(** [add name t file] records what [file] holds now, unless the power is cut.

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
