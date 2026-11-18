(** Does a directory answer as Linux or macOS does? *)

val scenarios : string list
(** [scenarios] is the name of each scenario, in the order they run. *)

val needs_user : string list
(** [needs_user] is the scenarios whose answers are those of a user other than
    root, with a supplementary group. *)

val check :
  ?os:[< `Linux | `Macos ] ->
  ?user:Eio_fs.user ->
  run:((Eio_chaos.env -> string list) -> (string list, string) result) ->
  (sw:Eio.Switch.t -> Eio_chaos.env -> _ Eio.Path.t) ->
  unit
(** [check ~os ~user ~run dir] is {!Eio_fs_chaos.device}, each scenario run by
    [run], which is its answers, or why its run did not end. *)

val eio_posix_differs : [< `Linux | `Macos ] -> string list
(** [eio_posix_differs os] is the scenarios where the recorded eio_posix answers
    on [os] differ from those {!check} expects. *)

val program :
  ?os:[< `Linux | `Macos ] ->
  ?user:Eio_fs.user ->
  (sw:Eio.Switch.t -> Eio_chaos.env -> _ Eio.Path.t) ->
  Eio_chaos.env ->
  unit
(** [program ~os ~user dir env] replays every scenario in turn, each in a
    directory of its own under [dir ~sw env], for a run of {!Eio_chaos.run}
    whose parts are those [dir] is built over. Until a part fails, an answer
    must be the kernel's; once one failed, an answer may also be an error eio-fs
    injects, and the scenario ends there. A restart replays every scenario
    again.

    @raise Failure
      with the line of {!check}'s report for the first answer outside that. *)
