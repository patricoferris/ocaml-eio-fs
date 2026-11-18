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

(* A path of the walk, with the names that lead to it from the root, last
   name first. *)
type 'a at = { rev_names : string list; path : 'a Eio.Path.t; follow : bool }

(* [f] on a path the filter accepts and its stat, then the entries below it
   when it is a directory. *)
let visit ~filter f { rev_names; path; follow } =
  if not (filter path) then []
  else
    let stat = Eio.Path.stat ~follow path in
    f (List.rev rev_names) path stat;
    if stat.kind <> `Directory then []
    else
      Eio.Path.read_dir path
      |> List.rev_map (fun name ->
          {
            rev_names = name :: rev_names;
            path = Eio.Path.(path / name);
            follow = false;
          })

let batch ~filter f ats =
  List.fold_left
    (fun next at -> List.rev_append (visit ~filter f at) next)
    [] ats

(* Partition the level first so the number of fibers is bounded structurally;
   each worker then performs its share sequentially. *)
let level ~max_fibers ~filter f ats =
  let worker_count = Int.min max_fibers (List.length ats) in
  let work = Array.make worker_count [] in
  List.iteri
    (fun i at ->
      let worker = i mod worker_count in
      work.(worker) <- at :: work.(worker))
    ats;
  match Array.to_list work with
  | [ ats ] -> batch ~filter f ats
  | batches -> List.concat (Eio.Fiber.List.map (batch ~filter f) batches)

let iter ?(max_fibers = 32) ?(follow = false) ?(filter = fun _ -> true) f path =
  let rec loop = function
    | [] -> ()
    | ats -> loop (level ~max_fibers ~filter f ats)
  in
  loop [ { rev_names = []; path; follow } ]
