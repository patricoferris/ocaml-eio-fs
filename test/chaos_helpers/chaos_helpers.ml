(* What the chaos, crash and writeback cases share. *)

module Chaos = Eio_fs.Chaos.Private
module Crash = Chaos.Crash

let ( / ) = Eio.Path.( / )
let i63 = Optint.Int63.of_int

(* [raw] is the un-perturbed base (for setup and for reading back what actually
   committed); [chaos] wraps the same in-memory tree with [schedule]. *)
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

(* [raw] is the un-perturbed base; [fs] is the same in-memory tree with
   durability tracked by [ctl]. After a crash, reads go through [raw] (or the
   handle [Crash.post_crash] returns), never through the dead [fs]. *)
let with_crash ctl fn =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs.run @@ fun env ->
  let raw = env#fs in
  let fs = Crash.wrap ctl raw in
  fn ~raw ~fs

let absent p = Eio.Path.kind ~follow:false p = `Not_found

(* Run [fn] over a crash-wrapped private temp directory on the real
   filesystem, then hand the recovered directory to [check]. *)
let with_real_crash ctl fn ~check =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let dir = Filename.temp_file "eio-fs-crash" "" in
  Sys.remove dir;
  Eio.Path.mkdir ~perm:0o755 Eio.Path.(fs / dir);
  let real = (Eio.Path.(fs / dir) :> Eio.Fs.dir_ty Eio.Path.t) in
  Fun.protect ~finally:(fun () ->
      Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / dir))
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
    Eio_fs_sync.sync_dir fs;
    workload fs;
    let post = Crash.post_crash ctl in
    Hashtbl.replace seen (Eio.Path.load (post / "f")) ()
  done;
  Hashtbl.fold (fun k () acc -> k :: acc) seen []
