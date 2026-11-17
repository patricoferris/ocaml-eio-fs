(** What the chaos, crash and writeback cases share. *)

val i63 : int -> Optint.Int63.t
(** [i63] is {!Optint.Int63.of_int}. *)

val at :
  ?nth:int ->
  Eio_fs_chaos.Op.kind ->
  string ->
  Eio_fs_chaos.Fault.t ->
  Eio_fs_chaos.Op.t ->
  Eio_fs_chaos.Fault.t option
(** [at ~nth kind path fault] answers [fault] for the [nth] (defaults to [1])
    operation of [kind] on [path], and nothing for the others. *)

val with_chaos :
  (Eio_fs_chaos.Op.t -> Eio_fs_chaos.Fault.t option) ->
  (raw:Eio.Fs.dir_ty Eio.Path.t -> chaos:Eio.Fs.dir_ty Eio.Path.t -> 'a) ->
  'a
(** [with_chaos f fn] calls [fn] with an in-memory tree, [raw], and the same
    tree wrapped by {!Eio_fs_chaos.wrap} with [f], [chaos]. *)

val contents : [> Eio.File.ro_ty ] Eio.File.ro -> string
(** [contents file] is the whole of [file], read through the handle. *)

val with_crash :
  Eio_fs_chaos.Private.Crash.t ->
  (raw:Eio.Fs.dir_ty Eio.Path.t ->
  fs:[ `Close | Eio.Fs.dir_ty ] Eio.Path.t ->
  'a) ->
  'a
(** [with_crash ctl fn] calls [fn] with an in-memory tree, [raw], and the same
    tree with its durability tracked by [ctl], [fs]. After a crash, reads go
    through [raw] or the handle [Crash.post_crash] returns. *)

val absent : [> Eio.Fs.dir_ty ] Eio.Path.t -> bool
(** [absent p] is [true] iff nothing is at [p]. *)

val with_real_dir : (Eio.Fs.dir_ty Eio.Path.t -> 'a) -> 'a
(** [with_real_dir fn] calls [fn] with a fresh private directory of the real
    filesystem, removed once [fn] returns. *)

val with_real_crash :
  Eio_fs_chaos.Private.Crash.t ->
  ([ `Close | Eio.Fs.dir_ty ] Eio.Path.t -> unit) ->
  check:(Eio.Fs.dir_ty Eio.Path.t -> 'a) ->
  'a
(** [with_real_crash ctl fn ~check] runs [fn] over a crash-wrapped private
    directory of the real filesystem, then hands the recovered directory to
    [check]. *)

val sync_dirs : [> Eio.Fs.dir_ty ] Eio.Path.t -> unit
(** [sync_dirs dir] syncs [dir] and every directory below it, parents first, so
    every directory change made below [dir] is durable. *)

val write_new : ?sync:bool -> [> Eio.Fs.dir_ty ] Eio.Path.t -> string -> unit
(** [write_new ~sync p data] creates [p] holding [data], synced with [sync]. *)

val overwrite : [> Eio.Fs.dir_ty ] Eio.Path.t -> string -> unit
(** [overwrite p data] writes [data] at the start of the existing [p]. *)

val images :
  ?block_size:int ->
  ([ `Close | Eio.Fs.dir_ty ] Eio.Path.t -> unit) ->
  string list
(** [images workload] is the distinct contents of [f] the seeds 0 to 199 of
    [Reorder_unsynced] rebuild after [workload]. *)
