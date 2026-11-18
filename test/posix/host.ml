let posix_only why f () =
  if Sys.win32 then begin
    Fmt.pr "skipped on Windows: %s@." why;
    Alcotest.skip ()
  end
  else f ()
