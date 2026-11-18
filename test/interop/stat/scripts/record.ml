(* Records each workload of [Stat_workload] in each directory named, one row
   per line of its answer: the workload, then the line in each directory, each
   separated by "|".

   record NAME=DIR...   records in a new subdirectory of each DIR *)

let usage () =
  prerr_endline "usage: record NAME=DIR...";
  exit 2

let () =
  let dirs =
    match List.tl (Array.to_list Sys.argv) with
    | [] -> usage ()
    | args ->
        List.map
          (fun a ->
            match String.split_on_char '=' a with
            | [ name; dir ] -> (name, dir)
            | _ -> usage ())
          args
  in
  Eio_posix.run @@ fun env ->
  (* Longer than a clock tick, so that a time that moves is seen to. *)
  let tick () = Eio.Time.sleep env#clock 0.05 in
  let fs = Eio.Stdenv.fs env in
  print_endline (String.concat "|" ("workload" :: List.map fst dirs));
  List.iter
    (fun (name, w) ->
      let answers =
        List.map
          (fun (_, d) ->
            let dir = Eio.Path.(fs / d / name) in
            Eio.Path.mkdir ~perm:0o755 dir;
            w ~tick dir)
          dirs
      in
      let rec rows = function
        | [] :: _ -> ()
        | answers ->
            print_endline (String.concat "|" (name :: List.map List.hd answers));
            rows (List.map List.tl answers)
      in
      rows answers)
    Stat_workload.all
