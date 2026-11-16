(** The metadata journal of a power-cut controller: its entries in program
    order, the indexes of the pending ones, and what a barrier commits. Pure: it
    knows inodes, not paths. *)

(** {1:entries Entries} *)

type 'a entry = private {
  seq : int;  (** The entry's place in program order. *)
  dirs : int64 list;  (** The directories whose barrier commits the entry. *)
  mutable barred : int64 list;
      (** Those of {!field-dirs} a barrier has reached since. *)
  attrs_ino : int64 option;
      (** For a change of attributes, the node it changed: a sync of that node
          commits it too. *)
  nodes : int64 list;  (** The nodes it makes. *)
  parents : int64 list;  (** The directories whose entries it changes. *)
  ancestors : int64 list;
      (** For a rename, the directories from the root down to where it lands. *)
  moved : int64 list;  (** The nodes it moves. *)
  removed : int64 list;  (** The nodes it removes. *)
  mutable synced : bool;  (** A sync of {!field-attrs_ino} has run since. *)
  clear : bool;
      (** A write's setuid or setgid clear: any later commit carries it. *)
  mutable committed : bool;
  op : 'a;  (** What the operation did. *)
}
(** The type for journal entries. *)

val entry :
  seq:int ->
  ?dirs:int64 list ->
  ?attrs_ino:int64 ->
  ?nodes:int64 list ->
  ?parents:int64 list ->
  ?ancestors:int64 list ->
  ?moved:int64 list ->
  ?removed:int64 list ->
  ?clear:bool ->
  'a ->
  'a entry
(** [entry ~seq op] is a pending entry no barrier or sync has reached. The lists
    default to empty and [clear] to [false]. *)

(** {1:journal Journals} *)

type 'a t
(** The type for journals. *)

val v : unit -> 'a t
(** [v ()] is an empty journal. *)

val reset : 'a t -> unit
(** [reset t] empties [t]. *)

val add : 'a t -> 'a entry -> unit
(** [add t e] appends [e] to [t]. The {!field-seq} of [e] is above every entry's
    of [t]. *)

val fold : ('a entry -> 'b -> 'b) -> 'a t -> 'b -> 'b
(** [fold f t acc] folds [f] over the entries of [t], in program order. *)

val made : 'a t -> int64 -> 'a entry option
(** [made t ino] is the last entry that made [ino], until {!forget}. *)

val forget : 'a t -> int64 -> unit
(** [forget t ino] drops which entry made [ino]: the snapshot holds it. *)

val pop_committed : 'a t -> 'a entry option
(** [pop_committed t] removes and is the first entry of [t] if it is committed.
*)

(** {1:deps Dependencies} *)

val deps : 'a t -> 'a entry -> 'a entry list
(** [deps t e] is the pending entries before [e] it depends on: those that
    changed a directory [e] changes, as one directory's entries stay in order,
    those that changed a directory [e] removes, and those that moved a directory
    above where [e] lands, so it never lands inside itself. A change of
    attributes pulls nothing in. *)

val closure : 'a t -> 'a entry -> 'a entry list
(** [closure t e] is [e] and, transitively, the pending entries it depends on.
*)

val clears_before : 'a t -> int -> 'a entry list
(** [clears_before t seq] is the pending setuid clears before [seq]. *)

(** {1:commits Barriers and commits} *)

val bar : ?upto:int -> 'a t -> int64 -> int
(** [bar ~upto t ino] holds a barrier on the directory [ino] for its pending
    entries up to [upto] (defaults to [max_int]), and is how many it newly
    reached. *)

val has_attrs : 'a t -> bool
(** [has_attrs t] is [true] iff [t] holds a pending change of attributes. *)

val sync_attrs : upto:int -> 'a t -> int64 -> int
(** [sync_attrs ~upto t ino] marks the pending changes of attributes of [ino] up
    to [upto] synced, and is how many it newly marked. *)

val settle :
  'a t -> reached:('a entry -> bool) -> commit:('a entry list -> unit) -> unit
(** [settle t ~reached ~commit] commits, until none is left, each pending entry
    a barrier or a sync reached for which [reached] holds, with its {!closure}
    and the setuid clears before the last of them, and calls [commit] with them,
    in program order. [reached] is asked again after each [commit]. *)
