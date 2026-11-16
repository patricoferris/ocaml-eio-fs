(** What a check is expected to do with a workload. *)

val field : string -> string -> string
(** [field name report] is the [name] line of a check's failure report. *)

val report : (unit -> unit) -> string
(** [report f] is the failure report of the check [f], which must fail. *)

val caught : tier:string -> (unit -> unit) -> unit -> unit
(** [caught ~tier f ()] tests that the check [f] fails on a scenario of [tier],
    ["linear"] or ["sampled"]. *)

val refused : string -> (unit -> unit) -> unit -> unit
(** [refused prefix f ()] tests that the check [f] fails with a report that
    starts with [prefix]: a guard, not a scenario. *)

val invalid : string -> (unit -> unit) -> unit -> unit
(** [invalid message f ()] tests that the check [f] refuses its arguments with
    [Invalid_argument message] before it runs. *)

val passes : (unit -> unit) -> unit -> unit
(** [passes f ()] tests that [f] returns. *)
