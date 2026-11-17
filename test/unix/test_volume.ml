(* What the volume holding the temporary directory uses, against the size and
   free space statvfs gives: the volume's own or, on APFS, its container's. *)

let with_tmp fn =
  Eio_main.run @@ fun env ->
  fn Eio.Path.(Eio.Stdenv.fs env / Filename.get_temp_dir_name ())

let bytes blocks (s : Eio_fs_unix.statvfs) = Int64.mul blocks s.block_size
let gib = Int64.shift_left 1L 30

(* Catches a figure of 0, and one that does not fit in the size. *)
let test_fits_its_container () =
  with_tmp @@ fun dir ->
  let used = Eio_fs_unix.volume_used dir in
  let s = Eio_fs_unix.statvfs dir in
  let total = bytes s.total_blocks s and free = bytes s.available_blocks s in
  Alcotest.(check bool)
    (Fmt.str "%Ld bytes used" used)
    true
    (Int64.compare used 0L > 0);
  Alcotest.(check bool)
    (Fmt.str "%Ld used and %Ld free fit in %Ld" used free total)
    true
    (Int64.compare (Int64.add used free) total <= 0)

(* On macOS the temporary directory is on the Data volume, in the APFS
   container that also holds the System volume: what the Data volume uses and
   what the container has free leave at least the System volume's gigabytes.
   Catches the container's used space given as the volume's, which statfs's
   blocks less its free blocks are. *)
let test_not_its_container () =
  if Sys.file_exists "/System/Volumes/Data" then
    with_tmp @@ fun dir ->
    let used = Eio_fs_unix.volume_used dir in
    let s = Eio_fs_unix.statvfs dir in
    let total = bytes s.total_blocks s and free = bytes s.available_blocks s in
    let others = Int64.sub total (Int64.add used free) in
    Alcotest.(check bool)
      (Fmt.str "%Ld used and %Ld free leave %Ld of %Ld to other volumes" used
         free others total)
      true
      (Int64.compare others gib >= 0)

(* [space] is statvfs's available bytes, as read a moment later, and the
   volume's used bytes. *)
let test_space () =
  with_tmp @@ fun dir ->
  let space = Eio_fs_unix.space dir in
  let s = Eio_fs_unix.statvfs dir in
  let total = bytes s.total_blocks s and free = bytes s.available_blocks s in
  Alcotest.(check bool)
    (Fmt.str "%Ld free, %Ld available a moment later" space.free free)
    true
    (Int64.compare (Int64.abs (Int64.sub space.free free)) gib < 0);
  Alcotest.(check bool)
    (Fmt.str "%Ld used and %Ld free fit in %Ld" space.used space.free total)
    true
    (Int64.compare space.used 0L > 0
    && Int64.compare (Int64.add space.used space.free) (Int64.add total gib)
       <= 0)

let suite =
  ( "volume",
    [
      Alcotest.test_case "used fits its container" `Quick
        test_fits_its_container;
      Alcotest.test_case "used is not the container's" `Quick
        test_not_its_container;
      Alcotest.test_case "space is free and used" `Quick test_space;
    ] )
