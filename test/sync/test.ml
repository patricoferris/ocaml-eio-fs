let () =
  Alcotest.run "eio-fs.sync"
    [
      Test_sync.suite; Test_sync.link_suite; Test_lock.suite; Test_rename.suite;
    ]
