(** Tests of the guards of [Eio_fs_chaos.run] and
    [Eio_fs_chaos.Private.check_cuts]. *)

val suite : string * unit Alcotest.test_case list
(** [suite] covers determinism, replay, re-matched, stale and unknown replays,
    the budget, the refusals, hangs, a run that retries at once or catches the
    cut, a cut during recover and a check inside another loop. *)
