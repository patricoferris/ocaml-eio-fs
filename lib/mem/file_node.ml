type contents = Literal of string | Buffer of Cstruct.t
type t = { stat : Eio.File.Stat.t; contents : contents; held : Extents.t }

let v ~stat = { stat; contents = Buffer Cstruct.empty; held = Extents.empty }

let literal ~stat contents =
  let n = String.length contents in
  { stat; contents = Literal contents; held = Extents.(add 0 n empty) }

let size f = Optint.Int63.to_int f.stat.size

let pp ppf f =
  Fmt.pf ppf "%d bytes, %d pages held" (size f) (Extents.pages f.held)

(* The 512-byte blocks of the 4096-byte pages a write put bytes in, as
   tmpfs counts them: a hole holds none. *)
let blocks f = Int64.of_int (Extents.pages f.held * (4096 / 512))

let blit t ~src_off dst ~dst_off ~len =
  match t with
  | Literal s -> Cstruct.blit_from_string s src_off dst dst_off len
  | Buffer b -> Cstruct.blit b src_off dst dst_off len

(* The largest file, 4 GiB, or [max_int] where an [int] cannot hold it:
   the bytes live in memory, holes included. *)
let max_size = if Sys.int_size > 33 then 1 lsl 32 else max_int

(* A buffer holding [f]'s bytes with room for [need], and every byte from
   [f]'s size to [need] zero: a truncation leaves stale bytes past the end. *)
let reserve f need =
  let size = size f in
  let buf =
    match f.contents with
    | Buffer b when Cstruct.length b >= need -> b
    | contents ->
        let cap =
          match contents with Buffer b -> Cstruct.length b | Literal _ -> 0
        in
        let b = Cstruct.create (max need (min max_size (2 * cap))) in
        blit contents ~src_off:0 b ~dst_off:0 ~len:size;
        b
  in
  if need > size then Cstruct.memset (Cstruct.sub buf size (need - size)) 0;
  buf

(* EFBIG past [max_size], checked without overflowing. *)
let check_size ~off ~len =
  if off > max_size || len > max_size - off then
    raise (Eio.Fs.err Eio.Fs.File_too_large)

let write f ~off src =
  let len = Cstruct.length src in
  check_size ~off ~len;
  let buf = reserve f (off + len) in
  Cstruct.blit src 0 buf off len;
  let size = Optint.Int63.of_int (max (size f) (off + len)) in
  {
    stat = { f.stat with size };
    contents = Buffer buf;
    held = Extents.add off (off + len) f.held;
  }

(* A buffer cut below a quarter of its capacity is reallocated, so a file
   emptied gives its memory back. *)
let truncate f n =
  check_size ~off:n ~len:0;
  let contents =
    match f.contents with
    | Buffer b when n <= size f && n < Cstruct.length b / 4 ->
        let small = Cstruct.create n in
        Cstruct.blit b 0 small 0 n;
        Buffer small
    | contents when n <= size f -> contents
    | _ -> Buffer (reserve f n)
  in
  {
    stat = { f.stat with size = Optint.Int63.of_int n };
    contents;
    held = Extents.cut n f.held;
  }
