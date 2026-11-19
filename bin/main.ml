open Cmdliner

let dir =
  Arg.(
    required
    & pos 0 (some dir) None
    & info [] ~docv:"DIR" ~doc:"The directory tree to embed.")

let output =
  Arg.(
    value
    & opt (some string) None
    & info [ "o"; "output" ] ~docv:"FILE.ml"
        ~doc:
          "Write the generated module to $(docv) and its interface to the \
           matching .mli, instead of printing the module to standard output.")

(* The walk Eio_fs_mem.load copies a tree with, its entries recorded from
   the image's root. *)
let scan dir : Eio_fs_mem.Crunch.entry list =
  Eio_fs_mem_core.Image.scan ~at:"/" dir
  |> List.map (function
    | Eio_fs_mem_core.Image.Dir { path; perm } ->
        Eio_fs_mem.Crunch.Dir { path; perm }
    | File { path; perm; contents } -> File { path; perm; contents }
    | Symlink { path; target } -> Symlink { path; target })

let run dir output =
  Eio_main.run @@ fun env ->
  match scan Eio.Path.(env#fs / dir) with
  | entries -> (
      match output with
      | None ->
          Fmt.pr "%a@?" Eio_fs_mem.Crunch.pp_ml entries;
          0
      | Some file when Filename.extension file <> ".ml" ->
          Fmt.epr "eio-fs-crunch: output file %s must end in .ml@." file;
          1
      | Some file ->
          let save name contents =
            Eio.Path.save ~create:(`Or_truncate 0o644)
              Eio.Path.(env#fs / name)
              contents
          in
          save file (Fmt.str "%a" Eio_fs_mem.Crunch.pp_ml entries);
          save
            (Filename.remove_extension file ^ ".mli")
            Eio_fs_mem.Crunch.interface;
          0)
  | exception (Eio.Io _ as e) ->
      Fmt.epr "eio-fs-crunch: %a@." Eio.Exn.pp e;
      1

let cmd =
  let doc = "Embed a directory tree as an OCaml module for eio-fs." in
  let man =
    [
      `S Manpage.s_description;
      `P
        "$(tname) walks $(i,DIR) and prints an OCaml module that embeds every \
         directory, file and symbolic link it contains. Linking the generated \
         module and calling its $(b,load \\(\\)) rebuilds the tree in a new \
         eio-fs in-memory filesystem, giving programs without a real \
         filesystem (unikernels, tests) an ordinary $(b,Eio.Path) view of \
         static assets.";
    ]
  in
  let info = Cmd.info "eio-fs-crunch" ~doc ~man in
  Cmd.v info Term.(const run $ dir $ output)

let () = exit (Cmd.eval' cmd)
