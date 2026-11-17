(** Where a directory answers differently from Linux or macOS. *)

val scenarios : string list
(** [scenarios] is the name of each scenario, in the order they run. *)

val check :
  ?os:[< `Linux | `Macos ] ->
  ?only:string list ->
  user:Low_level.User.t ->
  (unit -> _ Eio.Path.t) ->
  unit
(** See {!Eio_fs.Conformance.check}. *)

val eio_posix_differs : [< `Linux | `Macos ] -> string list
(** [eio_posix_differs os] is the scenarios where the recorded eio_posix answers
    on [os] differ from those {!check} expects. *)
