(** Does a directory answer as Linux or macOS does? *)

val scenarios : string list
(** [scenarios] is the name of each scenario, in the order they run. *)

val needs_user : string list
(** [needs_user] is the scenarios whose answers are those of a user other than
    root, with a supplementary group. *)

val check :
  ?os:[< `Linux | `Macos ] ->
  ?user:Eio_fs.user ->
  (sw:Eio.Switch.t -> _ Eio.Path.t) ->
  unit
(** See {!Eio_fs_chaos.device}. *)

val eio_posix_differs : [< `Linux | `Macos ] -> string list
(** [eio_posix_differs os] is the scenarios where the recorded eio_posix answers
    on [os] differ from those {!check} expects. *)
