(** The fault wrapper and the crash model that [Eio_fs.Chaos] is built on; the
    tests reach the crash model as [Eio_fs.Chaos.Private.Crash].

    {!wrap} wraps a directory, in memory or on disk, and asks a {!type-hook}
    about each read, write, sync, open, stat, listing and directory change. A
    fault is raised as an ordinary {!Eio.Io}, so the code under test cannot tell
    it from a failure of the base.

    {!Crash} models power loss: it tracks which writes and directory changes a
    barrier has made durable, and rebuilds the base as a cut leaves it. *)

type hook = space:bool -> Chaos_plan.Op.t -> Chaos_plan.Fault.t option
(** The type for what a wrapper asks about each operation. [space] is whether
    the operation may fail for want of room: a write, an open that makes its
    file, a [mkdir], a [symlink] or a [rename]. *)

type sched
(** The type for schedules: a hook and the numbering of the operations it is
    asked about, which every wrapper given the schedule shares. *)

val schedule : hook -> sched
(** [schedule f] asks [f] about each operation, numbered from [1]. *)

val applies : space:bool -> Chaos_plan.Op.t -> Chaos_plan.Fault.t -> bool
(** [applies ~space op f] is [true] iff [f] does something to [op]: a fault of
    another kind of operation does nothing, and neither does an [ENOSPC] on an
    open or a change that makes nothing, as Linux never answers it there. *)

val wrap : sched -> _ Eio.Path.t -> [ `Close | Eio.Fs.dir_ty ] Eio.Path.t
(** [wrap sched base] is [base] with each operation meeting the fault [sched]'s
    hook answers, when it {!applies}; opened files and subdirectories stay
    wrapped and share [sched].

    The wrapper and every file and subdirectory it opens hold [base] out of
    reach: each is a value of its own bound to the wrapper's interfaces only, so
    code handed the wrapper cannot rebuild a handler that reaches [base] and
    steps around [sched]. It answers {!Eio_fs_sync.Process_local} and
    {!Eio_fs_sync.Modeless} as [base] does, and every opened file, a read-only
    one included, registers {!Eio_fs_sync.File_sync} so a barrier through it
    reaches the wrapper's own sync. A backend's own interfaces are not carried
    over: a wrapper over a host directory is no native working directory for a
    child process, and its files hand out no host descriptor, except to
    [eio-fs.unix], which takes a lock on the descriptor beneath. A rename from
    an in-memory directory of {!Eio_fs.Fs} into a wrapper over the same tree is
    the in-memory rename, outside [sched]; one from a host directory of another
    backend into a wrapper is refused by that backend, which finds no directory
    of its own there. *)

(** A power-loss durability model over a base directory capability.

    Every write is tracked as unsynced until a successful {!Eio.File.sync} makes
    that file's prior writes durable. A file is known by its inode, not its
    path: a sync through any name of it, through a symbolic link, [.] or [..],
    or through a read-only handle, is a sync of that file, and its pending
    writes follow it through a rename of itself or of a directory above it.

    {b A failed sync loses the file's pending writes.} Linux marks the pages
    whose writeback failed clean without writing them, so a later sync that
    succeeds does not make them durable. A writeback may fail only in part, and
    the pages that succeeded are on disk: {!Crash.Reorder_unsynced} keeps any
    subset of the blocks failed syncs lost, each as one of those failed
    writebacks left it, and the other policies keep none. A truncation is not
    lost: ext4 journals it as metadata, so it stays pending and a later sync
    makes it durable. The page cache may still hold their bytes, and a later
    change that dirties one of those blocks then writes it whole, lost bytes
    included; or it may have evicted the page, and the later change lands over
    the bytes on disk. {!Crash.Reorder_unsynced} draws either for each file,
    whether a later sync ran or not; the other policies keep the page cache's.
    Before the failure the lost writes are pending like any write; after it, no
    crash shows them but through such a block. This holds for a sync failure
    {!wrap} injects above the model and for one the base raises beneath it. The
    only safe recovery is to write the data again from the application's own
    copy. A crash rewrites the base to a post-crash image in which unsynced
    writes may be dropped or reordered.

    {b Directory operations are journalled.} Creation, [mkdir], [unlink],
    [rmdir], [symlink], [rename], [chmod] and [chown] enter a metadata journal
    in program order; {!Crash.sync_metadata} commits it whole, and a directory
    barrier (a sync of the directory, opened as a file or not) commits only the
    entries of that directory and a [chmod] or [chown] of the directory itself,
    not one of an entry's. That is a deliberate weakening below ext4, whose
    barrier commits whole transactions in order: a child's [chmod] can be lost
    behind a sibling entry the barrier committed, which ext4 would not do and
    POSIX allows. A setuid clear, by contrast, is carried by any barrier that
    commits a later entry, and so is the length a truncation sets, which ext4
    journals in the syscall's own handle. A sync of a file also commits a
    [chmod] or [chown] of it. A write or a truncation that clears setuid or
    setgid enters the journal at once, at its place in program order, as ext4
    journals the change in the running transaction: a sync of the file commits
    it, as does {!Crash.sync_metadata} and any barrier that commits a later
    entry, and at a crash it survives with the prefix it is part of. A length
    the write, or a later write or truncation of the file, sets lands only with
    or after the clear, as ext4 journals both in program order; data that
    overwrites within the length may land without it, as ext4 allows. A barrier
    on a directory that is not itself durable yet holds from the moment it is,
    or from the moment another directory the entry changes is: with [d/f]
    durable, [mkdir e; rename d/f e/f; fsync_dir e] makes the rename durable
    when the barrier returns, so a crash before [e] itself is durable loses the
    file from both directories; so does [fsync_dir d] in place of [fsync_dir e].
    ext4 would keep [e/f], as committing the rename commits the [mkdir]; eio-fs
    deliberately weakens this, so that a missing barrier on [e]'s parent shows.
    The model knows files and directories by inode, as ext4 does: a barrier
    holds for its directory under whatever name it has later, and never for
    another directory of the same name. A barrier through a directory opened
    before it was removed or replaced commits that directory's own entries, and
    nothing of the one now at its name. Committed entries take effect in program
    order, whatever order their barriers ran in, and a barrier also commits the
    pending entries an entry it commits needs: the earlier ones in a directory
    it changes, as one directory's entries stay in order, the ones that emptied
    a directory it removes or a rename replaces, and, for a rename, the renames
    that moved a directory above where it lands, so a directory never lands
    inside itself. The entry that made a directory is not among them: only a
    barrier on its parent makes a new directory durable. Each entry takes effect
    whole or not at all: a creation whose directory the image does not hold
    makes nothing, and an rmdir of a directory the image still fills, or a
    rename over one, changes nothing. A rename moves the node the image holds,
    with what the image holds below it; a node made by a creation that made
    nothing still exists for a committed rename of it, which puts it where it
    moved, as its inode exists on disk; moved into a directory the image does
    not hold, a node leaves the image until a later committed rename brings it
    back. At a crash the policy decides how much of the uncommitted suffix
    survives, always a prefix of program order, and a dropped operation is
    rolled back whole; an entry a deferred barrier covers survives, with the
    entries it needs, whenever it would hold. *)
module Crash : sig
  (** The type for the kinds of unsynced change a journal entry records. *)
  type op_kind =
    | Data  (** A write; its offset and length locate it. *)
    | Truncate  (** A truncation; its offset is the new length. *)
    | Create  (** The open that created the file. *)
    | Mkdir  (** A new directory. *)
    | Unlink  (** A removed file or symbolic link. *)
    | Rmdir  (** A removed directory. *)
    | Symlink of { target : string }  (** A new symbolic link to [target]. *)
    | Rename of { dest : string }  (** An entry moved to [dest]. *)
    | Chmod of { perm : int }  (** A new mode. *)
    | Chown of { uid : int64; gid : int64 }  (** A new owner. *)

  type unsynced = {
    path : string;
        (** The path the change was made at: for a write or a truncation, the
            file's path then, with its symbolic links resolved; for a directory
            operation, the path as the caller gave it. A later rename does not
            change it. *)
    kind : op_kind;  (** What the change is. *)
    file_offset : int;
        (** The byte offset of a {!Data} write, the new length of a {!Truncate},
            and [0] for a directory operation. *)
    length : int;
        (** The byte length of a {!Data} write, and [0] for a {!Truncate} or a
            directory operation. *)
    seq : int;  (** The change's position in program order. *)
  }
  (** The type for unsynced changes. *)

  (** The type for how the post-crash image is rebuilt from the unsynced
      changes. *)
  type policy =
    | Drop_unsynced
        (** The state at the last barriers: every unsynced write is lost and
            every uncommitted directory operation is rolled back, but for the
            truncations made before a committed one. It is one crash state among
            many. It hides the bugs that need a later change to land, such as a
            rename that reaches the disk before the data it names;
            {!Reorder_unsynced} over many seeds finds those. *)
    | Reorder_unsynced of int
        (** Seeded: writes go back to the disk in blocks, as a page cache writes
            them. A write dirties the blocks it covers and, past the end of the
            file, those of the zeroed hole it leaves; a truncation that shrinks
            dirties only the block it cuts mid-way, none when it cuts on a
            boundary, and one that grows dirties the blocks it adds. Each dirty
            block comes back as it was durable or as it stood after any one of
            the changes that dirtied it, drawn independently of the other
            blocks. The length is drawn independently of the blocks: one a
            change left, or a block boundary a write grew the file across. A
            block a shrink did not dirty keeps its old bytes, which show again
            when a later length grows over them, as data=writeback may leave
            them. A shrink the length does not hold has freed no page: at that
            length the pages past the shrink hold no later change, as ext4
            reuses no block before the truncation that freed it commits. So a
            multi-block write may keep any subset of its blocks, while a block
            is never torn and never holds a later write without the earlier
            ones. Uncommitted directory operations survive as a prefix of
            program order whose length is drawn uniformly: metadata never
            reorders. A length a change left survives only with every directory
            operation before that change, as ext4 journals it as it writes the
            pages back, and a truncation survives with any directory operation
            after it that survives, as ext4 journals it in the syscall's own
            handle. The [int] is the seed; the same seed and workload reproduce
            the same image. *)
    | Survive of (unsynced -> bool)
        (** Scripted: an unsynced write survives iff the predicate holds;
            survivors keep program order. A disk writes a 512-byte sector whole,
            so where a surviving write overwrote bytes the file held, its sector
            holds what the page cache held after it, the earlier overwrites of
            that sector included. Otherwise writes are kept or dropped whole,
            whatever block they share. An uncommitted directory operation
            survives iff the predicate holds for it {e and} every earlier
            uncommitted directory operation survived (the prefix rule). A write
            or a truncation that changes the length survives only with every
            directory operation before it, and a truncation survives, whatever
            the predicate, with any directory operation after it that survives.
            It pins an exact scenario, for example keeping a generation pointer
            but dropping the data block it references. *)
    | Choose of (int -> int)
        (** As {!Reorder_unsynced}, with each draw [f n], in [\[0, n)], made by
            the function, so a caller can enumerate the states, and with lengths
            as ext4 leaves them: one a change left, or the end of a 4096-byte
            page a write put bytes in and grew the file across. A truncation
            also dirties the rest of the 4096-byte page it cuts, which ext4
            zeroes. A block a write put bytes in, in a 4096-byte page the disk
            did not hold, past the durable length or in a hole, may also hold
            stale data, as ext4 may commit the page before its bytes land: a
            page a durable truncation freed or a removed file held, the one at
            the same place first, then the others, up to eight distinct ones,
            and else every byte ['7']. Such a page holds bytes, its own or
            stale, only with every directory operation before the first write
            into it, as ext4 allocates it with the length. With 512-byte blocks,
            a disk's sectors, that is ext4 with [data=writeback]. *)

  type t
  (** The type for crash controllers. A controller tracks durability across the
      files opened through its wrapper and holds the crashed image after a
      crash. *)

  type base_dir = Eio.Fs.dir_ty Eio.Path.t
  (** The type for the backing store handed back after a crash. *)

  (** The type for what a crash image holds of a change that reached the store
      other than through the wrapper. *)
  type outside =
    | Dropped  (** Nothing: the image is rebuilt from the model alone. *)
    | Kept
        (** All of it, as durable, where the model holds no change; see
            [Chaos.Crash.wrap], whose controllers all keep it. *)

  val v :
    ?os:[< `Linux | `Macos ] ->
    ?policy:policy ->
    ?outside:outside ->
    ?block_size:int ->
    ?pause:(unit -> unit) ->
    unit ->
    t
  (** [v ~os ~policy ~outside ~block_size ~pause ()] is a fresh controller for
      [os]'s durability (defaults to [`Linux]) that rebuilds the post-crash
      image by [policy] (defaults to {!Drop_unsynced}, a single state) and
      [outside] (defaults to {!Dropped}). [block_size] (defaults to [4096]) is
      the byte size of the blocks {!Reorder_unsynced} writes back.

      A sync of a file or a directory covers the changes made before it, runs
      [pause ()] (defaults to doing nothing), then makes them durable. A change
      made during [pause], by another fiber, stays pending, as Linux's fsync(2)
      makes no promise about a write made while it runs.

      @raise Invalid_argument if [block_size] is not positive. *)

  val wrap : t -> _ Eio.Path.t -> [ `Close | Eio.Fs.dir_ty ] Eio.Path.t
  (** [wrap t base] is [base] with durability tracked by [t]; files and
      subdirectories opened through it stay wrapped and share [t]. The tree
      under [base] when [wrap] is called is [t]'s durable starting image.

      As for {!Chaos_model.wrap}, the wrapper and what it opens hold [base] out
      of reach, so every write that reaches [base] through them enters the
      journal. A file's writes stay unsynced until {!Eio.File.sync} or
      {!Eio_fs_sync.sync_file} reaches it through the wrapper. Under {!Dropped}
      a crash rebuilds the tree from the model, so a change made outside the
      wrapper, by name or on the host descriptor [eio-fs.unix] locks, is gone
      from the post-crash image; under {!Kept} it stays.

      The model reads and rebuilds the tree with root's rights, whatever user
      [base] runs as: on an in-memory directory of {!Eio_fs.Fs} it does so
      through the same directory as {!Eio_fs.root}. A host directory gives the
      model only the rights of the process, which must be able to read and write
      every entry of the tree, as its owner or root can.

      A controller has one wrapper, and its operations run as the user of
      [base]. {!Eio_fs.Fs.as_user} runs them as another user.

      @raise Invalid_argument
        if [t] is already wrapped, or if the tree under [base] holds an entry
        that is neither a regular file, a directory nor a symbolic link. *)

  val crash : t -> unit
  (** [crash t] triggers a crash now: it reconstructs the post-crash image per
      {!type-policy} and rewrites the base to it in place: an entry that is
      still the one the image names keeps its inode and times, as on a disk. A
      second call does nothing. After a crash, every operation through the
      wrapper (a read, a write, a sync, an open, a stat or a directory
      operation) fails with an {!Eio.Io} exception ("power lost") and leaves the
      image as it is; re-read through {!post_crash}.

      @raise Invalid_argument
        if [t] was never {!wrap}ped; [t] is left uncrashed. *)

  val is_crashed : t -> bool
  (** [is_crashed t] is [true] iff a crash has fired on [t]. *)

  val post_crash : t -> base_dir
  (** [post_crash t] crashes [t] if it has not already crashed, and is the base
      directory given to {!wrap}, which now holds the crashed image. It is that
      handle itself, not a new one, and no wrapper is in front of it, so a test
      re-reads the image through it.

      @raise Invalid_argument if [t] was never {!wrap}ped. *)

  val sync_metadata : t -> unit
  (** [sync_metadata t] commits the whole metadata journal: every pending
      directory operation becomes durable, as fsync(2) on every directory
      concerned would make it, a setuid or setgid clear included. A directory
      barrier ([Eio.Path.sync_dir] on a wrapped directory) commits only the
      entries of that directory.

      @raise Eio.Io ("power lost") if [t] has crashed. *)

  val left : t -> string list
  (** [left t] is, file by file, what [t]'s crash left of the unsynced writes
      under a {!Choose} policy: the length, and each run of sectors that holds
      something else than the durable bytes, with what it holds. *)

  val seq : t -> int
  (** [seq t] is the place in program order of the last change [t] tracked. *)

  val stamp : t -> int
  (** [stamp t] grows with each change [t] tracks, each commit, barrier or
      failure of a sync, and each file or mode an open meets: a sync or an open
      that leaves it as it was changed nothing a cut leaves. *)

  val unsynced : t -> unsynced list
  (** [unsynced t] are the changes not yet made durable, in program order: every
      write no sync made durable, and every journal entry not committed. That
      includes an entry a sync or a barrier reached that does not hold yet: a
      [chmod] or [chown] whose node is not durable, as one a file sync reached
      while the file's creation is pending or one a barrier on a new directory
      reached for that directory, and an entry whose directory is not durable.
      Such an entry survives a crash whenever it would hold, whatever a
      {!Survive} predicate says of it. *)

  val pp_unsynced : unsynced Fmt.t
  (** [pp_unsynced] formats an unsynced change for logs and test failures. *)
end

(**/**)

(* For this package's tests. It hands out nothing a schedule's holder could
   not reach by running an operation through a wrapper. *)
module Law = Chaos_law
(** What {!wrap} does with each operation of each interface it binds. *)

module Internal : sig
  val crash_image :
    Crash.t ->
    (string * int64 * [ `File of string | `Dir | `Link of string ]) list
  (** [crash_image t] is the image [t]'s crash rebuilt: each entry's path below
      the wrapped root, the inode it had before the crash, and what it holds. *)

  val descriptors : _ Eio.Path.t -> int option
  (** [descriptors path] is how many descriptors are open on the in-memory
      filesystem of [path], or [None] if [path] is not in one. *)

  val replay_copied : unit -> int
  (** [replay_copied ()] is how many bytes {!Crash} has copied into the images
      it replays a file's changes over, since the program started. *)
end
