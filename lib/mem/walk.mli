(** The one walk over a tree, behind [Eio_fs.Tree], [Eio_fs.Crunch] and
    [Eio_fs_mem.load]. *)

val iter :
  ?max_fibers:int ->
  ?follow:bool ->
  ?filter:('a Eio.Path.t -> bool) ->
  (string list -> 'a Eio.Path.t -> Eio.File.Stat.t -> unit) ->
  'a Eio.Path.t ->
  unit
(** [iter ~max_fibers ~follow ~filter f root] is [f names p stat] for [root] and
    each of its descendants [p], [names] the names that lead from [root] to [p]
    ([[]] for [root]) and [stat] the one stat of [p]. [root] is stated as
    [follow] (defaults to [false]) says, and every descendant without following
    it: a symbolic link below [root] is visited and never entered. [filter]
    (defaults to [fun _ -> true]) is asked about each path before its stat; a
    path it refuses is neither stated nor visited, nor anything under it. A
    directory's [f] runs before its entries are read. The walk is breadth-first,
    at most [max_fibers] workers per level (defaults to [32]), which must be
    positive.

    Filesystem errors and those [f] raises are propagated, the walk's other
    workers cancelled. *)
