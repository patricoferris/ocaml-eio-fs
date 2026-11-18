(*
 * Copyright (C) 2023 Thomas Leonard
 * Copyright (C) 2026 Memfs developers
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)
open! Hermetic

type error = [ `Invalid of string | `Too_large ]

let read_file ~max_bytes ~validate path =
  if max_bytes < 0 || max_bytes = max_int then
    Fmt.invalid_arg
      "Eio_fs.Nofollow.read_file: max_bytes is %d, must be between 0 and \
       max_int - 1"
      max_bytes;
  Eio.Switch.run @@ fun sw ->
  let file = Eio.Path.open_in ~sw ~follow:false path in
  Eio_fs_internal.bounded_read ~max_bytes ~validate ~stat:(Eio.File.stat file)
    ~read:(Eio.Flow.single_read file)

(* Once opened, a file may have lost its last name to a rename over it,
   which leaves the descriptor reading the file it opened. *)
let refused ~opened (s : Eio.File.Stat.t) =
  match s.kind with
  | `Regular_file when Int64.equal s.nlink 1L -> None
  | `Regular_file when opened && Int64.equal s.nlink 0L -> None
  | `Regular_file -> Fmt.kstr Option.some "a file of %Ld links" s.nlink
  | kind ->
      Fmt.kstr Option.some "a %a, not a regular file" Eio.File.Stat.pp_kind kind

let with_regular path f =
  match refused ~opened:false (Eio.Path.stat ~follow:false path) with
  | Some why -> Error (`Invalid why)
  | None -> (
      Eio.Path.with_open_in ~follow:false path @@ fun file ->
      let stat = Eio.File.stat file in
      match refused ~opened:true stat with
      | Some why -> Error (`Invalid why)
      | None -> Ok (f file stat))
