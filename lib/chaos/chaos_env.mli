(** The env a run of a check gets: the capabilities of [Eio.Stdenv], each
    simulated, so that every run of a scenario replays exactly. *)

type env =
  < fs : Eio.Fs.dir_ty Eio.Path.t
  ; cwd : Eio.Fs.dir_ty Eio.Path.t
  ; clock : float Eio.Time.clock_ty Eio.Resource.t
  ; mono_clock : Eio.Time.Mono.ty Eio.Resource.t
  ; secure_random : Eio.Flow.source_ty Eio.Resource.t
  ; stdin : Eio.Flow.source_ty Eio.Resource.t
  ; stdout : Eio.Flow.sink_ty Eio.Resource.t
  ; stderr : Eio.Flow.sink_ty Eio.Resource.t
  ; domain_mgr : Eio.Domain_manager.ty Eio.Resource.t
  ; process_mgr : [ `Generic ] Eio.Process.mgr_ty Eio.Resource.t
  ; debug : Eio.Debug.t
  ; backend_id : string >
(** The type for envs. *)

type side
(** The type for what the envs of one scenario share: how many were made, which
    seeds the entropy of the next, and what the program printed. *)

val side : unit -> side
(** [side ()] is a scenario's side, before its first env. *)

val v :
  side ->
  float Eio.Time.clock_ty Eio.Resource.t ->
  Eio.Time.Mono.ty Eio.Resource.t ->
  fs:_ Eio.Path.t ->
  cwd:_ Eio.Path.t ->
  env
(** [v side clock mono ~fs ~cwd] is the next env of [side]. Its [secure_random]
    and the default [Random] state, which [v] resets, are seeded by its place
    among the envs of [side], so the [n]th env of each scenario draws the same
    bytes. [stdin] is empty, [stdout] and [stderr] go to [side], [domain_mgr]
    runs its work in the calling fiber and [process_mgr] refuses with [Eio.Io].
*)

val within : side -> (unit -> 'a) -> 'a
(** [within side f] is [f ()] with [Eio.traceln] writing to [side]. *)

val mark : side -> string -> unit
(** [mark side line] adds [line] to what [side] printed, as a separator. *)

val printed : side -> string
(** [printed side] is what the programs of [side] printed, its last 2048 bytes
    or so, cut at a line. *)

val keeping_random : (unit -> 'a) -> 'a
(** [keeping_random f] is [f ()], with the default [Random] state as it was
    before. *)
