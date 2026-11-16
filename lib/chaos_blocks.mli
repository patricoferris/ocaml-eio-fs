(** A file's bytes under writeback, block by block: what a power cut may leave
    of the changes no sync made durable. Pure: it knows bytes, not paths. *)

(** {1:changes Changes} *)

(** The type for the changes to a file's bytes. *)
type change =
  | Write of { offset : int; data : Cstruct.t }
      (** [data] is a copy of the bytes written at [offset]. *)
  | Resize of int  (** A truncation to that length. *)

type image = { mutable buf : Cstruct.t; mutable len : int }
(** The type for a file's bytes while changes replay over them: the first [len]
    bytes of [buf], which grows by doubling. *)

val copy_cs : Cstruct.t -> Cstruct.t
(** [copy_cs c] is a fresh copy of [c]. *)

val copied : int Atomic.t
(** [copied] counts the bytes replays have copied into images. *)

val image_of : Cstruct.t -> image
(** [image_of c] is an image holding a copy of [c]. *)

val bytes_of : image -> Cstruct.t
(** [bytes_of img] is the bytes [img] holds, not copied. *)

val apply : image -> change -> unit
(** [apply img c] makes [c] on [img]; a write past the end leaves zeros. *)

val replay_changes : Cstruct.t -> change list -> Cstruct.t
(** [replay_changes durable cs] is [durable] with [cs] made over it, in order.
*)

(** {1:pages Pages and blocks} *)

val page : int
(** [page] is ext4's block, [4096] bytes, which it allocates whole. *)

val hold : Extents.t -> change -> Extents.t
(** [hold held c] is the byte ranges the disk holds pages of once [c] is
    durable: a write adds what it covers, a shrink frees the pages past it. *)

val held_at : Extents.t -> int -> int -> bool
(** [held_at held a b] is [true] iff the disk holds a page of [\[a, b)]. *)

val replay_cached :
  bs:int ->
  Cstruct.t ->
  cache:Cstruct.t option ->
  (change * bool) list ->
  Cstruct.t * Cstruct.t
(** [replay_cached ~bs durable ~cache cs] is [durable] with [cs] made over it,
    where a block of [bs] bytes a change dirties is written whole from the page
    cache [cache], which still holds what a failed sync lost; and the page cache
    after them. A change paired with [true] is one the cache already holds: it
    dirties no block. *)

val dirty_blocks : ?writes:bool -> bs:int -> int -> change list -> int list
(** [dirty_blocks ~writes ~bs len cs] is the blocks of [bs] bytes [cs] dirty,
    sorted, in a file of [len] bytes; with [writes] (defaults to [false]) those
    of its writes only. *)

(** {1:writeback Writeback} *)

type draw = { int : int -> int; bool : unit -> bool }
(** The type for the draws a crash state is made of: [int n] is in [\[0, n)]. *)

val draw_of_state : Random.State.t -> draw
(** [draw_of_state rng] draws from [rng]. *)

val draw_of_fun : (int -> int) -> draw
(** [draw_of_fun f] draws [f n], so a caller can enumerate the states. *)

val stale : char
(** [stale] is the byte that stands for stale data where the disk held nothing
    the model knows of. *)

val max_stale : int
(** [max_stale] is the most distinct freed pages a stale sector draws from. *)

val writeback :
  ?over:(int * Cstruct.t) list ->
  ?len_upto:int ->
  ?bytes:bool ->
  ?held:Extents.t ->
  ?old:(int -> (Cstruct.t * string) list) ->
  ?describe:
    (int ->
    (int
    * int
    * [ `Change of int | `Durable | `Over | `Stale of string option ])
    list ->
    unit) ->
  rng:draw ->
  bs:int ->
  cache:Cstruct.t option ->
  Cstruct.t ->
  (change * bool) list ->
  Cstruct.t
(** [writeback ~rng ~bs ~cache durable cs] is what a cut leaves of [durable]
    with the pending [cs] written back in blocks of [bs] bytes. Each block a
    change dirtied holds what it held when durable or after one of those
    changes, drawn from [rng]; the length is one a change left, or a block
    boundary a write grew the file across. A block is never torn, and never
    holds a later change without the earlier ones.

    [over] are blocks a failed writeback may have written, and their bytes;
    [len_upto] the changes, counted in order, that may set the length. With
    [bytes], blocks are sectors, the length is one a change left or the end of a
    page a write put bytes in, and a sector of a page the disk does not hold
    ([held]) may hold stale data: one of [old p] for the page [p], or
    {!val-stale}. [describe] is told the length and, run by run, where each
    block's bytes come from. *)

val sectors_together :
  cache:Cstruct.t option ->
  Cstruct.t ->
  (change * bool * bool) list ->
  Cstruct.t ->
  Cstruct.t
(** [sectors_together ~cache durable cs image] is [image] with the 512-byte
    sectors of each kept change, paired with [true] last, that an earlier lost
    change also overwrote holding what the page cache held after it, as a disk
    writes a sector whole. The middle boolean says the cache already holds the
    change. *)
