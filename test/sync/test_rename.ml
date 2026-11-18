module R = Eio_fs_sync.Rename

exception Refused

module Refusing = struct
  type t = (string * string) ref

  let rename seen (_, source) (_, target) =
    seen := (source, target);
    raise Refused
end

module Backend = R.Make (Refusing)

let test_unregistered_directory_is_refused () =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  match
    R.exclusive R.capability Eio.Path.(fs / "source") Eio.Path.(fs / "target")
  with
  | () -> Alcotest.fail "a missing exclusive-rename capability succeeded"
  | exception Invalid_argument message ->
      Alcotest.(check string)
        "missing operation named"
        (Fmt.str
           "Eio_fs_sync.Rename.exclusive: source is %a, in a directory with no \
            exclusive rename, pass a backend such as \
            Eio_fs_unix.exclusive_rename"
           Eio.Path.pp
           Eio.Path.(fs / "source"))
        message

let test_fallback_receives_paths_and_propagates_refusal () =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let seen = ref ("", "") in
  let backend = Backend.v seen in
  (match R.exclusive backend (fst fs, "/source") (fst fs, "/target") with
  | () -> Alcotest.fail "a backend refusal became success"
  | exception Refused -> ());
  Alcotest.(check (pair string string))
    "original paths reached the backend" ("/source", "/target") !seen

let test_registered_operation_precedes_fallback () =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs_mem.run @@ fun env ->
  let source = Eio.Path.(env#fs / "source") in
  let target = Eio.Path.(env#fs / "target") in
  Eio.Path.save ~create:(`Exclusive 0o600) source "payload";
  R.exclusive (Backend.v (ref ("", ""))) source target;
  Alcotest.(check string)
    "registered backend moved the bytes" "payload" (Eio.Path.load target);
  Alcotest.(check bool)
    "source name consumed" true
    (Eio.Path.kind ~follow:false source = `Not_found)

let suite =
  ( "rename",
    [
      Alcotest.test_case "unregistered directory is refused" `Quick
        test_unregistered_directory_is_refused;
      Alcotest.test_case "fallback receives paths and propagates refusal" `Quick
        test_fallback_receives_paths_and_propagates_refusal;
      Alcotest.test_case "registered operation precedes fallback" `Quick
        test_registered_operation_precedes_fallback;
    ] )
