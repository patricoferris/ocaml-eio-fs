(** The scenarios of a check, as pure functions of a recorded run. *)

(** The operations a wrapper sees. *)
module Op : sig
  type kind =
    [ `Open_read
    | `Open_write
    | `Stat
    | `Read
    | `Read_dir
    | `Write
    | `Truncate
    | `Sync
    | `Change ]

  type t = {
    index : int;
    kind : kind;
    path : string;
    nth : int;
    file_offset : int;
    length : int;
    dest : string option;
  }

  val kind_name : kind -> string
  (** [kind_name k] names [k] in reports and keys. *)

  val pp : t Fmt.t
  (** [pp] formats an operation. *)
end

(** The faults a wrapper injects. *)
module Fault : sig
  type code = [ `Enoent | `Eacces | `Eperm | `Erofs | `Enospc | `Eio ]

  type t =
    | Fail of code
    | Evict of code
    | Short of int
    | Torn of int
    | Swap_symlink of string

  val pp : t Fmt.t
  (** [pp] formats a fault. *)
end

type pos = { kind : Op.kind; path : string; nth : int }
(** The type for the place of an operation in a run, by its kind, path and rank
    among those, so it is found again in another run. *)

val at : pos -> Op.t -> bool
(** [at p op] is [true] iff [op] is at [p]. *)

val pos_text : ?at:(pos -> Op.t option) -> pos -> string
(** [pos_text ~at p] says where [p] is, for a report: as the operation [at p] of
    the run is shown in its trace, and else by its place among the operations of
    its kind on its path. *)

type trace = Op.t list
(** The type for a run's operations, in order. *)

val structural : trace -> trace
(** [structural t] is the operations of [t] that make its structure: on each
    file, the first three reads and writes, the last, and the first of each
    length class, a length rounded down to a power of two; the syncs that follow
    them; and every other operation. *)

val sample : Random.State.t -> int -> 'a list -> 'a list
(** [sample rng n l] is [l] if it has [n] or fewer elements. Otherwise it splits
    [l] into [n] runs of consecutive elements whose lengths differ by one at
    most, and is one element of each run, drawn with [rng], in the order of [l].
    Unlike every [k]-th element, it meets each element of [l] for some state of
    [rng], whatever the period of [l]. *)

val interleave : Random.State.t -> 'a list -> 'a list
(** [interleave rng l] is [l] in an order each prefix of which is spread over
    it: when the length of [l] is a power of two, its first [m] elements, for
    each power of two [m], hold one element of each of [m] runs of consecutive
    elements of equal length. [rng] draws which, so any element may come first.
*)

val sampled_share : budget:int -> linear:int -> classes:int -> int
(** [sampled_share ~budget ~linear ~classes] is the runs of the sampled tier for
    each of its [classes]: in all at least a fortieth of [budget], and up to
    half of [linear], the runs the linear tier took, within [budget]. *)

(** {1 check} *)

(** The type for the conditions that last from an operation on. *)
type lasting = Read_only | Full | Denied

type check = { faults : (pos * Fault.t) list; lasting : (lasting * pos) option }
(** The type for a scenario of [check]: faults that fire once each, in order, or
    a lasting condition. *)

val none : check
(** [none] is the scenario with no fault. *)

val lasting_code : lasting -> Op.t -> Fault.code option
(** [lasting_code l op] is the error [op] meets under [l], if any. *)

val check_linear : trace -> check list
(** [check_linear first] is each fault alone at each structural operation of
    [first], and each lasting condition from each; then each structural sync
    failed with its pages evicted. *)

val is_evict : pos * Fault.t -> bool
(** [is_evict f] is [true] iff [f] fails a sync and evicts its pages. The linear
    tier alone tries it, so no second fault follows it. *)

val check_others : trace -> check list
(** [check_others first] is the same at the other operations. *)

val check_others_sample :
  keep:(check -> bool) -> trace -> int * (Random.State.t -> int -> check list)
(** [check_others_sample ~keep first] is the number of scenarios of
    [check_others first] for which [keep] holds, and a function that is [n] of
    them drawn with [rng], as {!sample} draws them, built only as they are
    drawn. *)

(** The operations sure to be structural as a run goes. *)
module Sure : sig
  type t
  (** The type for what a run made so far. *)

  val v : unit -> t
  (** [v ()] is a run that made nothing yet. *)

  val add : t -> Op.t -> bool
  (** [add t op] adds [op] to [t], and is [true] iff {!structural} keeps [op] in
      every trace that starts as [t] does. *)
end

val check_floor : space:bool -> sure:bool -> Op.t -> int
(** [check_floor ~space ~sure op] is the scenarios {!check_linear} has at [op]
    in every trace that has [op], given whether [op] needs room and is sure to
    be structural: none if it is not. Their sum over a run so far is a floor of
    the tier's size. *)

(** The changes a run made to the disk. *)
module Disk : sig
  type t
  (** The type for the writes, truncations, syncs, directory changes and opens
      that may write of a run, in order. *)

  val v : trace -> t
  (** [v first] is those of [first]. *)
end

val check_seconds :
  ?limit:int ->
  rng:Random.State.t ->
  pos * Fault.t ->
  first:Disk.t ->
  after:trace ->
  check list * check list
(** [check_seconds ~limit ~rng f ~first ~after] is the pairs of faults whose
    first is [f] and whose second lands after it in [after], the run [f] alone
    made, best first, at most [limit] (defaults to all). If [f] changed what the
    run does to the disk, past [f], from [first], the run with no fault, they
    are the first list, which aims at the path [f] sent the run down: the
    operations that change the disk, then the others, each nearest [f] first.
    Else they are the second list, drawn with [rng] as {!sample} draws them, in
    the order {!interleave} gives. With no [limit], both lists are every pair,
    whatever [rng]. *)

(** The pairs of faults the sampled tier tries: those of each first fault, best
    first, round by round. *)
module Pairs : sig
  type 'a t
  (** The type for pairs kept. *)

  val v : int -> 'a t
  (** [v n] keeps at most [n] pairs. *)

  val add : 'a t -> 'a list * 'a list -> unit
  (** [add t (changed, others)] adds the pairs of one first fault, best first:
      those on an operation it changed, and the others. Past [n], it drops one
      of the others of the largest rank, else one of the changed of the largest
      rank, the latest added first: each first fault keeps as many as the
      others, but for those that have fewer, those on what they changed first.
  *)

  val to_list : 'a t -> 'a list
  (** [to_list t] is the pairs kept: on a changed operation by rank, then the
      others by rank, each rank in the order added. *)
end

val faults_text : ?at:(pos -> Op.t option) -> (pos * Fault.t) list -> string
(** [faults_text fs] says what [fs] do, for a report. *)

val check_key : check -> string
(** [check_key s] names [s] in a replay. *)

val check_text : ?at:(pos -> Op.t option) -> check -> string
(** [check_text s] says what [s] does, for a report. *)

(** {1 Power cuts} *)

(** The type for the state a cut leaves. *)
type state =
  | Drop  (** Every unsynced change lost. *)
  | Keep  (** Every unsynced change kept. *)
  | Enum of int list
      (** One of the states the cut allows, by the draws that build it. *)
  | Seed of int  (** One drawn at random from a seed. *)
  | Shape of int list
      (** One drawn with a shape, given by the draws that build it. *)

val shape : widths:int list -> int -> state
(** [shape ~widths i] is the [i]th state drawn at random for a cut whose draws,
    in the state where each takes its first choice, have [widths] choices: a
    fourth of them from a seed, and the others with a shape, as writeback leaves
    one: a prefix of the draws taking their last choice and the rest their
    first, every draw its first but one, or every draw its last but one. *)

type crash = {
  cfaults : (pos * Fault.t) list;
  cut : pos option;  (** Before that operation; [None] at the end. *)
  state : state;
  rcut : (pos option * state) option;  (** A cut during recover. *)
}
(** The type for a scenario of power cuts. *)

val base : crash
(** [base] is the cut at the end that loses every unsynced change. *)

val tears : sizes:(string * int) list -> trace -> Op.t -> int list
(** [tears ~sizes first op] is where the write [op] of [first] may be torn,
    given the size of each file before [first]: at the 512-byte sectors of the
    bytes it overwrites, and anywhere in those it adds. *)

val crash_cuts : changed:(Op.t -> bool) -> trace -> crash list
(** [crash_cuts ~changed first] is a cut after each structural operation of
    [first] that changes the disk, and at its end, in both simplest states. A
    write, a truncation or a directory change changes it; another operation only
    if [changed] holds for it: a sync or an open that changed the controller, as
    one that commits something or meets a file the first time, or an operation
    an ack followed. A cut after another sync or open leaves what the cut before
    it leaves, as nothing a cut reads changed. *)

val crash_floor : sure:bool -> changed:bool -> Op.t -> int
(** [crash_floor ~sure ~changed op] is the runs the linear tier of power cuts
    has for [op] in every trace where an operation follows it, as {!check_floor}
    is for {!type-check} scenarios. It is none unless [op] is sure to be
    structural. Otherwise it is six if [op] changes the disk, as {!crash_cuts}
    reads [changed]: a cut after it, in two states, each with the two cuts at
    the end of recover. A sync adds four: its failure and its failure with its
    pages evicted, each in two states. *)

val crash_faults : tears:(Op.t -> int list) -> trace -> crash list
(** [crash_faults ~tears first] is each structural sync failed and each
    structural write torn, alone, with a cut at the end; then each structural
    sync failed with its pages evicted. *)

val crash_others :
  changed:(Op.t -> bool) -> tears:(Op.t -> int list) -> trace -> crash list
(** [crash_others ~changed ~tears first] is the same at the other operations. *)

val rcuts : changed:(Op.t -> bool) -> crash -> recover:trace -> crash list
(** [rcuts ~changed s ~recover] is [s] with a cut after each structural change
    of [recover], the trace of its recovery, and at its end. *)

val crash_seconds :
  ?limit:int ->
  rng:Random.State.t ->
  tears:(Op.t -> int list) ->
  pos * Fault.t ->
  first:Disk.t ->
  after:trace ->
  crash list * crash list
(** [crash_seconds ~limit ~rng ~tears f ~first ~after] is the pairs of faults
    whose first is [f] and whose second lands after it in [after]. *)

val next_choices : (int * int) list -> int list option
(** [next_choices trail] is the draws of the next state a cut allows, after the
    one whose draws, last first, are [trail], each with the number of choices it
    had; [None] after the last. *)

(** The draws the states of a cut make. *)
module Draws : sig
  type t
  (** The type for the tree of the draws met so far. *)

  val empty : unit -> t
  (** [empty ()] is a tree with no draw. *)

  val add : t -> (int * int) list -> unit
  (** [add t trail] adds the draws, last first, of one state. *)

  val estimate : t -> int
  (** [estimate t] is the number of states the draws met so far allow, a choice
      not yet taken counted as the largest one taken at its draw. *)
end

val state_key : state -> string
(** [state_key st] names [st] in a replay. *)

val crash_key : crash -> string
(** [crash_key s] names [s] in a replay. *)

val well_formed : string -> bool
(** [well_formed k] is [true] iff [k] has the shape of a key {!crash_key} makes,
    its states as {!state_key} names them. *)

val split_state_key : string -> (string * state) option
(** [split_state_key k] is the key of the scenario [k] names with its state
    replaced by {!constructor-Drop}, and that state. *)
