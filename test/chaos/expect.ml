let field name report =
  let prefix = "  " ^ name ^ ":" in
  String.split_on_char '\n' report
  |> List.find_map (fun l ->
      if String.starts_with ~prefix l then
        let n = String.length prefix in
        Some (String.trim (String.sub l n (String.length l - n)))
      else None)
  |> Option.value ~default:""

let report f =
  match f () with
  | () -> Alcotest.fail "the check passed: it did not catch the bug"
  | exception Failure m -> m

let caught f () =
  let m = report f in
  if field "replay" m = "" then
    Alcotest.failf "the check failed for another reason: %s" m

let refused prefix f () =
  let m = report f in
  if not (String.starts_with ~prefix m) then
    Alcotest.failf "the check failed with %S" m

let invalid message f () =
  match f () with
  | () -> Alcotest.fail "the check passed: it did not refuse its argument"
  | exception Invalid_argument m -> Alcotest.(check string) "refusal" message m

let passes f () = f ()
