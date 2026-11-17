let () =
  Alcotest.run "eio-fs"
    [
      Test_eio_fs.suite;
      Test_nofollow.suite;
      Test_low_level.suite;
      Test_memfs.suite;
      Test_chaos.suite;
      Test_crash.suite;
      Test_writeback.suite;
      Test_sync_dir.suite;
    ]
