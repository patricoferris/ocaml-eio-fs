(* A log of fixed-size records, one write each. A write that fails is
   rolled back by truncating the log to where it began; with [bug], a write
   in the middle of the log is not, as if a failed write left nothing. Only
   a torn write there leaves part of a record. *)

module P = Eio.Path

let size = 8
let record i = Fmt.str "%0*d\n" (size - 1) i

let log ?(bug = false) ~n (env : Eio_fs.Chaos.env) =
  let path = P.(env#fs / "log") in
  let whole =
    try
      P.with_open_out ~create:(`Exclusive 0o644) path @@ fun f ->
      let rec append i at =
        if i <= n then
          match Eio.Flow.copy_string (record i) f with
          | () -> append (i + 1) (at + size)
          | exception Eio.Io _ ->
              if not (bug && i > 32 && i < n - 32) then
                Eio.File.truncate f (Optint.Int63.of_int at)
      in
      append 1 0;
      true
    with Eio.Io _ -> false
  in
  if whole then
    match (P.stat ~follow:false path).size with
    | s ->
        let s = Optint.Int63.to_int s in
        if s mod size <> 0 then Fmt.failwith "the log has %d bytes" s
    | exception Eio.Io _ -> ()
