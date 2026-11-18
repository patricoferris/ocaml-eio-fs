(* Win32 paths through the unrestricted fs: the in-memory filesystem under
   [`Windows] answers what eio_windows answered on Windows, recorded in
   traces/paths.csv. An error compares by its class, as the kernel traces
   do; the trace's last column, Win32's own answer, is for the reader. *)

let work = {|C:\tmp\paths|}
let other = {|D:\elsewhere|}

(* A row without its last column, each error by its class alone. *)
let comparable row =
  let cols = String.split_on_char ',' row in
  let cols = List.filteri (fun i _ -> i < 6) cols in
  let outcome o =
    match String.index_opt o ':' with
    | Some i
      when List.mem (String.sub o 0 i) [ "ENOENT"; "EEXIST"; "denied"; "exn" ]
      ->
        String.sub o 0 i
    | _ -> o
  in
  List.mapi
    (fun i c ->
      if i = 5 then
        String.concat "|" (List.map outcome (String.split_on_char '|' c))
      else c)
    cols

let recorded () =
  In_channel.with_open_bin
    (Filename.concat "traces" "paths.csv")
    In_channel.input_lines
  |> List.tl

let test_paths () =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs_mem.run ~os:`Windows ~drives:[ 'C'; 'D' ] ~cwd:work @@ fun env ->
  Eio.Path.mkdir ~perm:0o755 Eio.Path.(env#fs / other);
  let got = Win_paths.rows ~fs:env#fs ~work ~other () in
  List.iter2
    (fun want got ->
      Alcotest.(check (list string))
        (List.hd (String.split_on_char ',' want))
        (comparable want) (comparable got))
    (recorded ()) got

let () =
  Alcotest.run "eio-fs.windows"
    [ ("paths", [ Alcotest.test_case "as on Windows" `Quick test_paths ]) ]
