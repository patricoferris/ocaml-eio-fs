let read file = In_channel.with_open_bin file In_channel.input_all

let lines file =
  In_channel.with_open_text file In_channel.input_lines
  |> List.filter_map (fun line ->
      match String.index_opt line ':' with
      | Some i when String.starts_with ~prefix:"# " line ->
          Some
            ( String.sub line 2 (i - 2),
              String.trim (String.sub line (i + 1) (String.length line - i - 1))
            )
      | _ -> None)

let measured v =
  v <> "" && v <> "--" && not (String.starts_with ~prefix:"unknown" v)

let pinned_image script =
  let prefix = "image=\"" in
  match
    List.find_opt
      (String.starts_with ~prefix)
      (String.split_on_char '\n' (read script))
  with
  | Some l ->
      let n = String.length prefix in
      String.sub l n (String.length l - n - 1)
  | None -> Alcotest.failf "%s pins no image" script

let check ~trace ~keys ~code ~script =
  let meta = lines trace in
  let get k =
    match List.assoc_opt k meta with
    | Some v when measured v -> v
    | _ -> Alcotest.failf "%s has no measured %S: regenerate it" trace k
  in
  List.iter (fun k -> ignore (get k)) keys;
  Alcotest.(check string)
    (Fmt.str "recorded by %s as they are" (String.concat " and " code))
    (Digest.to_hex (Digest.string (String.concat "" (List.map read code))))
    (get "script digest");
  Alcotest.(check string)
    (Fmt.str "recorded in the image %s pins" script)
    (pinned_image script) (get "linux image")
