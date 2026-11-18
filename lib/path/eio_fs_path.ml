(*---------------------------------------------------------------------------
   Copyright (c) 2015 The fpath programmers. All rights reserved.
   SPDX-License-Identifier: ISC

   This is a modified Fpath to only be POSIX which complies with Eio's path
   policies too.
  ---------------------------------------------------------------------------*)

(* Adapted from src/path.ml in eio_mem (https://git.sirref.org/eio_mem); see
   LICENSE.md. A library of its own, so a caller that needs only what a path
   resolves to links no filesystem, no terminal and no C stubs. *)

open! Hermetic

let is_abs p = String.length p > 0 && p.[0] = '/'
let segs p = String.split_on_char '/' p

let is_directory_syntax p =
  let len = String.length p in
  len > 0
  && (p.[len - 1] = '/'
     || match List.rev (segs p) with "." :: _ | ".." :: _ -> true | _ -> false)

let normalize p =
  let absolute = is_abs p in
  let rec loop acc = function
    | [] -> List.rev acc
    | ("" | ".") :: rest -> loop acc rest
    | ".." :: rest -> (
        match acc with
        | segment :: acc when not (String.equal segment "..") -> loop acc rest
        | [] when absolute -> loop [] rest
        | acc -> loop (".." :: acc) rest)
    | segment :: rest -> loop (segment :: acc) rest
  in
  let directory = is_directory_syntax p in
  let body = String.concat "/" (loop [] (segs p)) in
  let path =
    match (absolute, body) with
    | true, "" -> "/"
    | true, body -> "/" ^ body
    | false, "" -> "."
    | false, body -> body
  in
  if directory && not (String.equal path "/") then path ^ "/" else path

type refusal = Not_a_path | Relative | Root | Not_canonical

let traversal segment = String.equal segment "." || String.equal segment ".."

let canonical path =
  if String.equal path "" || String.contains path '\000' then Error Not_a_path
  else if not (is_abs path) then Error Relative
  else
    let parts =
      List.filter (fun segment -> not (String.equal segment "")) (segs path)
    in
    if parts = [] then Error Root
    else if List.exists traversal parts then Error Not_canonical
      (* The components must rejoin to the string given: a trailing, doubled
         or leading double separator does not. *)
    else if not (String.equal ("/" ^ String.concat "/" parts) path) then
      Error Not_canonical
    else Ok parts
