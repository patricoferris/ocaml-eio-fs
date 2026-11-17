open Lock

external fstatat : string -> bool -> At.raw_stat = "caml_eio_fs_inline_fstatat"

external openat : string -> int -> int -> Unix.file_descr
  = "caml_eio_fs_inline_openat"

external readdir : Unix.dir_handle -> Eio.File.Stat.kind * string
  = "caml_eio_fs_inline_readdir"

let stat_path ~follow path =
  At.stat_of_raw (eio_unix_errors (fun () -> fstatat path follow))

(* The bits caml_eio_fs_inline_openat reads. *)
let flags ~write ~append ~follow (create : Eio.Fs.create) =
  let bit b n = if b then n else 0 in
  let create =
    match create with
    | `Never -> 0
    | `If_missing _ -> 4
    | `Or_truncate _ -> 4 lor 8
    | `Exclusive _ -> 4 lor 16
  in
  bit write 1 lor bit append 2 lor create lor bit (not follow) 32

let perm : Eio.Fs.create -> int = function
  | `Never -> 0
  | `If_missing p | `Or_truncate p | `Exclusive p -> p

let open_fd ~write ~append ~follow ~create path =
  eio_unix_errors (fun () ->
      openat path (flags ~write ~append ~follow create) (perm create))

(* The calls of [B] whose system calls this module makes on the calling
   thread; the others are [B]'s own. *)
module Make (B : Eio.Fs.Pi.DIR) = struct
  include B

  (* The path the unconfined root resolves from the working directory, as
     its own calls resolve it. *)
  let resolve t path =
    match B.native t path with
    | Some p -> p
    | None -> invalid_arg "Eio_fs_unix.inline_fs: a path with no native form"

  include B

  let stat t ~follow path = stat_path ~follow (resolve t path)

  let open_in t ~sw ~follow path =
    let fd =
      open_fd ~write:false ~append:false ~follow ~create:`Never (resolve t path)
    in
    match Unix.fstat fd with
    | { st_kind = S_FIFO; _ } ->
        (* A FIFO is read as the root reads one, awaiting its writer. *)
        Unix.close fd;
        B.open_in t ~sw ~follow path
    | _ ->
        (Eio_unix.File.import_ro ~sw ~close_unix:true fd
          :> Eio.File.ro_ty Eio.Resource.t)
    | exception e ->
        Unix.close fd;
        raise e

  let open_out t ~sw ~follow ~append ~create path =
    let fd = open_fd ~write:true ~append ~follow ~create (resolve t path) in
    (Eio_unix.File.import_rw ~sw ~close_unix:true fd
      :> Eio.File.rw_ty Eio.Resource.t)

  (* A name in directory syntax is resolved by the root, which strips
     its slash as the backend does. *)
  let read_link t path =
    if String.ends_with ~suffix:"/" path then B.read_link t path
    else eio_unix_errors (fun () -> Unix.readlink (resolve t path))

  (* The names but [.] and [..], as eio_posix lists them before
     Eio.Path.read_dir sorts them. *)
  let read_dir t path =
    let path = match resolve t path with "" -> "." | p -> p in
    eio_unix_errors @@ fun () ->
    let h = Unix.opendir path in
    let rec names acc =
      match Unix.readdir h with
      | "." | ".." -> names acc
      | name -> names (name :: acc)
      | exception End_of_file -> acc
    in
    Fun.protect ~finally:(fun () -> Unix.closedir h) (fun () -> names [])

  (* [fn] of the entries but [.] and [..], each with the kind the
     directory records for it, read as [fn] takes them. *)
  let with_dir_entries t path fn =
    let path = match resolve t path with "" -> "." | p -> p in
    let h = eio_unix_errors (fun () -> Unix.opendir path) in
    let rec next () =
      match eio_unix_errors (fun () -> readdir h) with
      | _, ("." | "..") -> next ()
      | entry -> Some entry
      | exception End_of_file -> None
    in
    Fun.protect
      ~finally:(fun () -> Unix.closedir h)
      (fun () -> fn (Seq.of_dispenser next))
end

let fs ((Eio.Resource.T (bv, bops), root) : Eio.Fs.dir_ty Eio.Path.t) =
  let module B = (val Eio.Resource.get bops Eio.Fs.Pi.Dir) in
  let module O = Make (B) in
  let ops =
    Eio.Resource.handler
      (Eio.Resource.H (Eio.Fs.Pi.Dir, (module O)) :: Eio.Resource.bindings bops)
  in
  ((Eio.Resource.T (bv, ops) :> Eio.Fs.dir_ty Eio.Resource.t), root)
