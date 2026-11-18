module P = Eio_fs.Path

let refusal =
  Alcotest.testable
    (fun ppf -> function
      | P.Not_a_path -> Fmt.string ppf "not a path"
      | P.Relative -> Fmt.string ppf "relative"
      | P.Root -> Fmt.string ppf "the root"
      | P.Not_canonical -> Fmt.string ppf "not canonical")
    ( = )

let answer = Alcotest.(result (list string) refusal)

let accepts path parts =
  Alcotest.check answer
    (Fmt.str "%S is canonical" path)
    (Ok parts) (P.canonical path)

let refuses why path =
  Alcotest.check answer
    (Fmt.str "%S is %a" path (Alcotest.pp refusal) why)
    (Error why) (P.canonical path)

(* A canonical path is the one spelling of itself: what comes back rejoins to
   the string that went in, so the caller's name for the object and the
   filesystem's are the same string. *)
let test_a_canonical_path_is_its_own_spelling () =
  accepts "/a" [ "a" ];
  accepts "/srv/app/logs" [ "srv"; "app"; "logs" ];
  accepts "/a/..b/c." [ "a"; "..b"; "c." ];
  (* A backslash and a colon are ordinary characters in this language, whatever
     a host path module would make of them. *)
  accepts "/a\\b/c:d" [ "a\\b"; "c:d" ]

(* Every other spelling of the same object. Each names what the canonical one
   names and splits differently, which is the whole reason to refuse it. *)
let test_every_other_spelling_is_refused () =
  refuses P.Not_canonical "/a//b";
  refuses P.Not_canonical "//a/b";
  refuses P.Not_canonical "/a/b/";
  refuses P.Not_canonical "/a/b//";
  refuses P.Not_canonical "/a/./b";
  refuses P.Not_canonical "/a/../b";
  refuses P.Not_canonical "/..";
  refuses P.Not_canonical "/."

(* The three answers that are not about spelling at all. *)
let test_a_path_with_no_components_and_a_path_that_is_not_one () =
  refuses P.Relative "a/b";
  refuses P.Relative "./a";
  refuses P.Relative "..";
  refuses P.Root "/";
  refuses P.Root "//";
  refuses P.Not_a_path "";
  (* Every layer below takes a NUL for the end of the string, so the path
     acted on would be [/srv/app]. *)
  refuses P.Not_a_path "/srv/app\000/../../etc";
  refuses P.Not_a_path "\000"

(* A canonical path is its own normal form. *)
let test_a_canonical_path_is_normal () =
  List.iter
    (fun path ->
      Alcotest.(check string)
        (Fmt.str "normalize %S" path)
        path (P.normalize path))
    [ "/a"; "/srv/app/logs"; "/a/..b/c." ]

let suite =
  ( "path",
    [
      Alcotest.test_case "a canonical path is its own spelling" `Quick
        test_a_canonical_path_is_its_own_spelling;
      Alcotest.test_case "every other spelling is refused" `Quick
        test_every_other_spelling_is_refused;
      Alcotest.test_case "no components, or not a path" `Quick
        test_a_path_with_no_components_and_a_path_that_is_not_one;
      Alcotest.test_case "a canonical path is normal" `Quick
        test_a_canonical_path_is_normal;
    ] )
