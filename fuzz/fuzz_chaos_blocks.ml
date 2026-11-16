open Alcobar
open Chaos_blocks

(* Small offsets, lengths and block sizes, so changes share blocks. *)
let change =
  choose
    [
      map
        [ range 40; range 16; range 256 ]
        (fun offset n c ->
          Write
            { offset; data = Cstruct.of_string (String.make n (Char.chr c)) });
      map [ range 48 ] (fun m -> Resize m);
    ]

let pp_change ppf = function
  | Write { offset; data } -> Fmt.pf ppf "w(%d,%d)" offset (Cstruct.length data)
  | Resize m -> Fmt.pf ppf "t(%d)" m

let pp_cs ppf c = Fmt.pf ppf "%S" (Cstruct.to_string c)
let pending changes = List.map (fun c -> (c, false)) changes

let image ~draw ~bs durable changes =
  writeback ~rng:draw ~bs ~cache:None
    (Cstruct.of_string durable)
    (pending changes)

(* The file after the first [k] changes. *)
let version durable changes k =
  replay_changes
    (Cstruct.of_string durable)
    (List.filteri (fun i _ -> i < k) changes)

(* Every draw the last choice: every change lands, as a process kill leaves
   it. *)
let test_keep_all durable changes bs =
  let draw = { int = (fun n -> n - 1); bool = (fun () -> true) } in
  check_eq ~pp:pp_cs ~eq:Cstruct.equal
    (image ~draw ~bs durable changes)
    (replay_changes (Cstruct.of_string durable) changes)

(* Every draw the first choice: the bytes as they were durable. *)
let test_drop_all durable changes bs =
  let draw = { int = (fun _ -> 0); bool = (fun () -> true) } in
  check_eq ~pp:pp_cs ~eq:Cstruct.equal
    (image ~draw ~bs durable changes)
    (Cstruct.of_string durable)

(* Any draws: a length some prefix of the changes left, or a block boundary
   below the longest, and each block as one prefix of the changes left it,
   never torn. *)
let test_prefix_blocks durable changes bs seed =
  let draw = draw_of_state (Random.State.make [| seed |]) in
  let img = image ~draw ~bs durable changes in
  let n = List.length changes in
  let versions = List.init (n + 1) (version durable changes) in
  let size = Cstruct.length img in
  let sizes = List.map Cstruct.length versions in
  let longest = List.fold_left max 0 sizes in
  if not (List.mem size sizes || (size mod bs = 0 && size <= longest)) then
    failf "length %d, not one of %a" size Fmt.(Dump.list int) sizes;
  let block c b =
    let off = b * bs in
    let len = min bs (size - off) in
    let got = Cstruct.create len in
    let n = max 0 (min len (Cstruct.length c - off)) in
    if n > 0 then Cstruct.blit c off got 0 n;
    got
  in
  if size > 0 then
    for b = 0 to (size - 1) / bs do
      let got = block img b in
      if not (List.exists (fun v -> Cstruct.equal (block v b) got) versions)
      then
        failf "block %d is %a, as no prefix of %a left it" b pp_cs got
          Fmt.(Dump.list pp_change)
          changes
    done

(* Only the first [k] changes may set the length: one they left, or a block
   boundary below the longest. In bytes, on a new file, no page holds a
   byte unless one of them is a write: ext4 allocates the page with the
   length. *)
let test_len_upto durable changes bs k seed =
  let draw = draw_of_state (Random.State.make [| seed |]) in
  let img =
    writeback ~len_upto:k ~rng:draw ~bs ~cache:None
      (Cstruct.of_string durable)
      (pending changes)
  in
  let sizes =
    List.init
      (min k (List.length changes) + 1)
      (fun j -> Cstruct.length (version durable changes j))
  in
  let longest = List.fold_left max 0 sizes and size = Cstruct.length img in
  if not (List.mem size sizes || (size mod bs = 0 && size <= longest)) then
    failf "length %d, not one of %a" size Fmt.(Dump.list int) sizes;
  let img =
    writeback ~len_upto:k ~bytes:true ~rng:draw ~bs:512 ~cache:None
      Cstruct.empty (pending changes)
  in
  let written = function Write _ -> true | Resize _ -> false in
  if
    (not (List.exists written (List.filteri (fun i _ -> i < k) changes)))
    && String.exists (fun c -> c <> '\000') (Cstruct.to_string img)
  then failf "a page no write up to %d filled holds %a" k pp_cs img

let block_size = range ~min:1 8

let suite =
  ( "chaos_blocks",
    [
      test_case "every change kept is the live file"
        [ bytes; list change; block_size ]
        test_keep_all;
      test_case "every change lost is the durable file"
        [ bytes; list change; block_size ]
        test_drop_all;
      test_case "each block is one a prefix of the changes left"
        [ bytes; list change; block_size; int ]
        test_prefix_blocks;
      test_case "a change past len_upto sets no length and fills no new page"
        [ bytes; list change; block_size; range 6; int ]
        test_len_upto;
    ] )
