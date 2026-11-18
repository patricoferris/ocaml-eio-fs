(* Records what eio_windows answers for Win32 paths through the unrestricted
   fs (Win_paths), in the current directory, which must be empty, with
   OTHER, an empty directory on another drive. A read through OCaml's
   Stdlib, which goes through the C runtime to CreateFileW, is what Win32
   itself makes of a path.

   paths [OTHER] *)

let win32 path =
  try String.escaped (In_channel.with_open_bin path In_channel.input_all)
  with Sys_error e -> "Sys_error: " ^ e

let () =
  set_binary_mode_out stdout true;
  let other =
    match Sys.argv with
    | [| _ |] -> None
    | [| _; other |] -> Some other
    | _ ->
        prerr_endline "usage: paths [OTHER]";
        exit 2
  in
  Eio_windows.run @@ fun env ->
  print_endline Win_paths.header;
  List.iter print_endline
    (Win_paths.rows ~fs:(Eio.Stdenv.fs env) ~work:(Sys.getcwd ()) ?other ~win32
       ())
