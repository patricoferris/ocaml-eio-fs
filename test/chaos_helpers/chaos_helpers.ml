module Chaos = Eio_fs.Chaos.Private
module Crash = Chaos.Crash

let ( / ) = Eio.Path.( / )
let i63 = Optint.Int63.of_int

let with_chaos schedule fn =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs.run @@ fun env ->
  let raw = env#fs in
  let chaos = Chaos.wrap schedule raw in
  fn ~raw ~chaos

(* The whole of a file, read through the handle rather than through its name. *)
let contents file =
  let len = Optint.Int63.to_int (Eio.File.size file) in
  let buf = Cstruct.create len in
  Eio.File.pread_exact file ~file_offset:(i63 0) [ buf ];
  Cstruct.to_string buf

let with_crash ctl fn =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs.run @@ fun env ->
  let raw = env#fs in
  let fs = Crash.wrap ctl raw in
  fn ~raw ~fs

let absent p = Eio.Path.kind ~follow:false p = `Not_found

let with_real_crash ctl fn ~check =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let dir = Filename.temp_file "eio-fs-crash" "" in
  Sys.remove dir;
  let dir = Eio.Path.(fs / dir) in
  Eio.Path.mkdir ~perm:0o755 dir;
  let real = (dir :> Eio.Fs.dir_ty Eio.Path.t) in
  Fun.protect ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true dir)
  @@ fun () ->
  let wrapped = Crash.wrap ctl real in
  fn wrapped;
  check (Crash.post_crash ctl)

let write_new ?(sync = false) p data =
  Eio.Path.with_open_out ~create:(`Exclusive 0o644) p (fun f ->
      Eio.File.pwrite_all f ~file_offset:(i63 0) [ Cstruct.of_string data ];
      if sync then Eio.File.sync f)

let overwrite p data =
  Eio.Path.with_open_out ~create:`Never p (fun f ->
      Eio.File.pwrite_all f ~file_offset:(i63 0) [ Cstruct.of_string data ])

(* The images seeds 0 to 199 rebuild of one workload. *)
let images ?block_size workload =
  let seen = Hashtbl.create 8 in
  for seed = 0 to 199 do
    let ctl = Crash.v ?block_size ~policy:(Reorder_unsynced seed) () in
    with_crash ctl @@ fun ~raw:_ ~fs ->
    write_new ~sync:true (fs / "f") "";
    Eio.Path.sync_dir fs;
    workload fs;
    let post = Crash.post_crash ctl in
    Hashtbl.replace seen (Eio.Path.load (post / "f")) ()
  done;
  Hashtbl.fold (fun k () acc -> k :: acc) seen []
