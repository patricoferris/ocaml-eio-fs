(* The tests of the chaos API's design, and its mutants: each program under
   test takes a bug, and each test says which tier of the check finds it. *)

let () =
  Alcotest.run "eio-fs.chaos"
    [
      Test_after.suite;
      Test_store.suite;
      Test_programs.suite;
      Test_guards.suite;
      Test_states.suite;
      Test_time.suite;
      Test_env.suite;
      Test_macos.suite;
      Test_moved.suite;
      Test_sampling.suite;
      Test_restart.suite;
      Test_ack.suite;
      Test_under.suite;
    ]
