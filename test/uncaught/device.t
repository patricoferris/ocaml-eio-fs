An uncaught device report prints as a run's does: a table of the
scenarios that differ, and a summary.

  $ ./device.exe
  Fatal error: exception Eio_fs_chaos.device: 1 of 112 scenarios differ from macOS
    scenario     operations         expected  got
    ------------------------------------------------
    case lookup  write a x; stat A  file 1    ENOENT
    111 passed, 1 differed, 3 left out: 3 that need a native path
  [2]
