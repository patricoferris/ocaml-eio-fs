open! Hermetic

type (_, _, _) Eio.Resource.pi +=
  | Base_dir :
      ('t, 't -> Eio.Fs.dir_ty Eio.Resource.t, [> `Base_dir ]) Eio.Resource.pi
  | Base_file :
      ('t, 't -> Eio.File.ro_ty Eio.Resource.t, [> `Base_file ]) Eio.Resource.pi
  | Admin :
      ('t, 't -> Eio.Fs.dir_ty Eio.Resource.t, [> `Admin ]) Eio.Resource.pi
  | Sync_failed :
      ('t, 't -> evict:bool -> unit, [> `Sync_failed ]) Eio.Resource.pi
  | Rebase :
      ( 't,
        't ->
        (Eio.Fs.dir_ty Eio.Resource.t ->
        [ `Close | Eio.Fs.dir_ty ] Eio.Resource.t) ->
        [ `Close | Eio.Fs.dir_ty ] Eio.Resource.t,
        [> `Rebase ] )
      Eio.Resource.pi

let base_dir (Eio.Resource.T (t, ops)) =
  Option.map (fun get -> get t) (Eio.Resource.get_opt ops Base_dir)

let base_file (Eio.Resource.T (t, ops)) =
  Option.map (fun get -> get t) (Eio.Resource.get_opt ops Base_file)

let admin (Eio.Resource.T (t, ops)) =
  Option.map (fun get -> get t) (Eio.Resource.get_opt ops Admin)

let rebase (Eio.Resource.T (t, ops)) =
  Option.map (fun get -> get t) (Eio.Resource.get_opt ops Rebase)

let bounded_read ~max_bytes ~validate ~stat ~read =
  match validate stat with
  | Error reason -> Error (`Invalid reason)
  | Ok () ->
      (* The buffer starts at the size the descriptor reports, one byte over
         so the end is told from growth, and doubles toward the bound only
         while the file keeps answering: a read costs the file, not the
         bound. A buffer outgrown is wiped as it is dropped. *)
      let limit = max_bytes + 1 in
      let size = Optint.Int63.to_int stat.Eio.File.Stat.size in
      let buffer = ref (Cstruct.create (min limit (max 0 size + 1))) in
      let returned = ref false in
      Fun.protect
        ~finally:(fun () -> if not !returned then Cstruct.memset !buffer 0)
        (fun () ->
          let grow filled =
            let larger =
              Cstruct.create (min limit (2 * Cstruct.length !buffer))
            in
            Cstruct.blit !buffer 0 larger 0 filled;
            Cstruct.memset !buffer 0;
            buffer := larger
          in
          let rec fill filled =
            if filled = Cstruct.length !buffer then
              if filled = limit then filled
              else begin
                grow filled;
                fill filled
              end
            else
              let dst = Cstruct.shift !buffer filled in
              match read dst with
              | 0 -> filled
              | got -> fill (filled + got)
              | exception End_of_file -> filled
          in
          let length = fill 0 in
          if length > max_bytes then Error `Too_large
          else begin
            returned := true;
            Ok (Cstruct.sub !buffer 0 length)
          end)

let without_trailing_slashes path =
  let rec last_non_slash i =
    if i <= 0 || path.[i] <> '/' then i else last_non_slash (i - 1)
  in
  let last = last_non_slash (String.length path - 1) in
  if last < 0 then path else String.sub path 0 (last + 1)

let basename path =
  let path = without_trailing_slashes path in
  if String.equal path "/" then ""
  else
    let name =
      match String.rindex_opt path '/' with
      | None -> path
      | Some index ->
          String.sub path (index + 1) (String.length path - index - 1)
    in
    match name with "." | ".." -> "" | name -> name

let native_name path =
  match Eio.Path.native path with
  | Some native -> native
  | None -> Format.asprintf "%a" Eio.Path.pp path

let last_component path =
  match basename path with
  | "" -> if String.equal path "" then "." else path
  | base -> base

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

let walk ?(max_fibers = 32) ?(follow = false) ?(filter = fun _ -> true) f path =
  let rec loop = function
    | [] -> ()
    | ats -> loop (level ~max_fibers ~filter f ats)
  in
  loop [ { rev_names = []; path; follow } ]
