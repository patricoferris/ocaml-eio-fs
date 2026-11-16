(* Extents on chosen edges; fuzz/fuzz_extents.ml checks it against a
   reference on random sequences. *)

let ranges ?from t =
  List.rev (Extents.fold ?from (fun a b l -> (a, b) :: l) t [])

let of_list l =
  List.fold_left (fun t (a, b) -> Extents.add a b t) Extents.empty l

let pair = Alcotest.(list (pair int int))

let test_add () =
  Alcotest.check pair "an empty range adds nothing" []
    (ranges (of_list [ (5, 5); (7, 3) ]));
  Alcotest.check pair "ranges that touch merge"
    [ (0, 20) ]
    (ranges (of_list [ (10, 20); (0, 10) ]));
  Alcotest.check pair "ranges apart stay apart"
    [ (0, 1); (2, 3) ]
    (ranges (of_list [ (2, 3); (0, 1) ]));
  Alcotest.check pair "a range across several merges them all"
    [ (0, 40) ]
    (ranges (of_list [ (0, 5); (10, 15); (20, 25); (30, 40); (3, 32) ]));
  Alcotest.check pair "a range inside another changes nothing"
    [ (0, 100) ]
    (ranges (of_list [ (0, 100); (10, 20) ]))

let test_cut () =
  let t = of_list [ (0, 10); (20, 30); (40, 50) ] in
  Alcotest.check pair "across a range"
    [ (0, 10); (20, 25) ]
    (ranges (Extents.cut 25 t));
  Alcotest.check pair "at a range's start"
    [ (0, 10) ]
    (ranges (Extents.cut 20 t));
  Alcotest.check pair "at a range's end"
    [ (0, 10); (20, 30) ]
    (ranges (Extents.cut 30 t));
  Alcotest.check pair "at zero" [] (ranges (Extents.cut 0 t));
  Alcotest.check pair "past the end" (ranges t) (ranges (Extents.cut 1000 t))

let test_overlaps () =
  let t = of_list [ (10, 20) ] in
  Alcotest.(check bool) "ending at its start" false (Extents.overlaps 0 10 t);
  Alcotest.(check bool) "starting at its end" false (Extents.overlaps 20 30 t);
  Alcotest.(check bool) "one byte in" true (Extents.overlaps 19 30 t);
  Alcotest.(check bool) "inside" true (Extents.overlaps 12 13 t);
  Alcotest.(check bool) "around it" true (Extents.overlaps 0 30 t)

let test_pages () =
  let pages l = Extents.pages (of_list l) in
  Alcotest.(check int) "none" 0 (pages []);
  Alcotest.(check int) "one byte" 1 (pages [ (4095, 4096) ]);
  Alcotest.(check int) "across a page boundary" 2 (pages [ (4095, 4097) ]);
  Alcotest.(check int) "two ranges in one page" 1 (pages [ (0, 1); (10, 11) ]);
  Alcotest.(check int)
    "neighbours sharing a page" 3
    (pages [ (0, 4097); (4100, 8193) ]);
  Alcotest.(check int)
    "a cut frees the pages past it" 1
    (Extents.pages (Extents.cut 4096 (of_list [ (0, 3 * 4096) ])))

let test_fold_from () =
  let t = of_list [ (0, 10); (20, 30); (40, 50) ] in
  Alcotest.check pair "from inside a range keeps it whole"
    [ (20, 30); (40, 50) ]
    (ranges ~from:25 t);
  Alcotest.check pair "from a range's end skips it"
    [ (40, 50) ]
    (ranges ~from:30 t);
  Alcotest.check pair "from past the last" [] (ranges ~from:50 t)

let suite =
  ( "extents",
    [
      Alcotest.test_case "add" `Quick test_add;
      Alcotest.test_case "cut" `Quick test_cut;
      Alcotest.test_case "overlaps" `Quick test_overlaps;
      Alcotest.test_case "pages" `Quick test_pages;
      Alcotest.test_case "fold from" `Quick test_fold_from;
    ] )
