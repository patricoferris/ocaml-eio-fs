(* Quick check: [dune build @fuzz]; with AFL: [crow start --cpus=4].
   Extents, Blocks and Journal are private to eio_fs: their
   sources are compiled in here. *)

let () =
  Alcobar.run "eio-fs"
    [
      Fuzz_chaos.suite;
      Fuzz_eio_fs_sync.suite;
      Fuzz_extents.suite;
      Fuzz_chaos_blocks.suite;
      Fuzz_chaos_journal.suite;
    ]
