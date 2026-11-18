let nfd us =
  let n = Uunf.create `NFD in
  let out = ref [] in
  let rec add v =
    match Uunf.add n v with
    | `Uchar u ->
        out := u :: !out;
        add `Await
    | `Await | `End -> ()
  in
  List.iter (fun u -> add (`Uchar u)) us;
  add `End;
  List.rev !out

let uchars s =
  let rec go i acc =
    if i >= String.length s then List.rev acc
    else
      let d = String.get_utf_8_uchar s i in
      go (i + Uchar.utf_decode_length d) (Uchar.utf_decode_uchar d :: acc)
  in
  go 0 []

(* NTFS's upcase table: each character's simple uppercase. *)
let upper u =
  match Uucp.Case.Map.to_upper u with
  | `Uchars [ u' ] -> u'
  | `Self | `Uchars _ -> u

let key os name =
  match os with
  | `Linux -> name
  | `Windows ->
      let b = Buffer.create (String.length name) in
      List.iter (fun u -> Buffer.add_utf_8_uchar b (upper u)) (uchars name);
      Buffer.contents b
  | `Macos when not (String.is_valid_utf_8 name) -> name
  | `Macos ->
      let folded =
        List.concat_map
          (fun u ->
            match Uucp.Case.Fold.fold u with `Self -> [ u ] | `Uchars us -> us)
          (nfd (uchars name))
      in
      let b = Buffer.create (String.length name) in
      List.iter (Buffer.add_utf_8_uchar b) (nfd folded);
      Buffer.contents b

(* NAME_MAX: 255 bytes on Linux, 255 UTF-16 code units on APFS and NTFS. *)
let too_long os name =
  match os with
  | (`Macos | `Windows) when String.is_valid_utf_8 name ->
      List.fold_left
        (fun n u -> n + (Uchar.utf_16_byte_length u / 2))
        0 (uchars name)
      > 255
  | `Linux | `Macos | `Windows -> String.length name > 255
