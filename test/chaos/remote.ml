type flow = [ `Generic ] Eio.Net.stream_socket_ty Eio.Resource.t

type t = {
  env : Eio_chaos.env;
  sw : Eio.Switch.t;
  leak : bool;
  calls : (int, unit -> unit) Hashtbl.t;
  mutable next : int;
  mutable conn : (flow * Eio.Buf_read.t) option;
}

let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 7000)

let eio what =
  Eio.Exn.create (Eio.Exn.X (Eio_unix.Unix_error (Unix.EIO, "remote", what)))

let connection t =
  match t.conn with
  | Some c -> c
  | None ->
      let flow = (Eio.Net.connect ~sw:t.sw t.env#net addr :> flow) in
      let c = (flow, Eio.Buf_read.of_flow ~max_size:64 flow) in
      t.conn <- Some c;
      c

(* The request [f] run by the server: its result, or its exception. *)
let call t what f =
  let n = t.next in
  t.next <- n + 1;
  let result = ref None in
  Hashtbl.replace t.calls n (fun () ->
      result :=
        Some
          (match f () with
          | v -> Ok v
          | exception (Eio.Cancel.Cancelled _ as e) -> raise e
          | exception e -> Error e));
  let answered =
    match
      Eio.Time.with_timeout t.env#clock 5.0 (fun () ->
          let flow, r = connection t in
          Eio.Flow.copy_string (Fmt.str "%d\n" n) flow;
          Ok (Eio.Buf_read.line r = string_of_int n))
    with
    | Ok true -> Ok ()
    | Ok false -> Error (eio what)
    | Error `Timeout -> Error (eio what)
    | exception ((Eio.Io _ | End_of_file) as e) ->
        Error (if t.leak then e else eio what)
  in
  Hashtbl.remove t.calls n;
  match (answered, !result) with
  | Ok (), Some (Ok v) -> v
  | Ok (), Some (Error e) -> raise e
  | Ok (), None -> raise (eio what)
  | Error e, _ ->
      t.conn <- None;
      raise e

let serve t l =
  Eio.Net.run_server l ~on_error:ignore (fun flow _ ->
      let r = Eio.Buf_read.of_flow ~max_size:64 flow in
      let rec loop () =
        let line = Eio.Buf_read.line r in
        Option.iter
          (fun f -> f ())
          (Option.bind (int_of_string_opt line) (Hashtbl.find_opt t.calls));
        Eio.Flow.copy_string (line ^ "\n") flow;
        loop ()
      in
      try loop () with End_of_file -> ())

(* {1 Files} *)

module Ro = struct
  type nonrec t = { r : t; f : Eio.File.ro_ty Eio.Resource.t; what : string }

  let call t f = call t.r t.what f
  let single_read t buf = call t (fun () -> Eio.Flow.single_read t.f buf)
  let read_methods = []

  let pread t ~file_offset bufs =
    call t (fun () -> Eio.File.pread t.f ~file_offset bufs)

  let stat t = call t (fun () -> Eio.File.stat t.f)
  let seek t off whence = call t (fun () -> Eio.File.seek t.f off whence)
  let close t = Eio.Resource.close t.f
end

module Rw = struct
  type t = { ro : Ro.t; w : Eio.File.rw_ty Eio.Resource.t }

  let single_read t = Ro.single_read t.ro
  let read_methods = []
  let pread t = Ro.pread t.ro
  let stat t = Ro.stat t.ro
  let seek t = Ro.seek t.ro
  let close t = Ro.close t.ro
  let call t f = Ro.call t.ro f
  let single_write t bufs = call t (fun () -> Eio.Flow.single_write t.w bufs)
  let copy t ~src = Eio.Flow.Pi.simple_copy ~single_write t ~src

  let pwrite t ~file_offset bufs =
    call t (fun () -> Eio.File.pwrite_single t.w ~file_offset bufs)

  let sync t = call t (fun () -> Eio.File.sync t.w)
  let truncate t n = call t (fun () -> Eio.File.truncate t.w n)
end

let ro_handler = Eio.File.Pi.ro (module Ro)
let rw_handler = Eio.File.Pi.rw (module Rw)

(* {1 Directories} *)

type dir = { r : t; base : Eio.Fs.dir_ty Eio.Path.t; close : unit -> unit }

type (_, _, _) Eio.Resource.pi +=
  | Base : ('t, 't -> Eio.Fs.dir_ty Eio.Path.t, [> `Remote ]) Eio.Resource.pi

module rec Dir : (Eio.Fs.Pi.DIR with type t = dir) = struct
  type t = dir

  let at d path = Eio.Path.(d.base / path)
  let call d path f = call d.r path f

  let open_in d ~sw ~follow path =
    let f = call d path (fun () -> Eio.Path.open_in ~sw ~follow (at d path)) in
    Eio.Resource.T ({ Ro.r = d.r; f; what = path }, ro_handler)

  let open_out d ~sw ~follow ~append ~create path =
    let w =
      call d path (fun () ->
          Eio.Path.open_out ~sw ~follow ~append ~create (at d path))
    in
    let f = (w :> Eio.File.ro_ty Eio.Resource.t) in
    Eio.Resource.T ({ Rw.ro = { Ro.r = d.r; f; what = path }; w }, rw_handler)

  let mkdir d ~perm path =
    call d path (fun () -> Eio.Path.mkdir ~perm (at d path))

  let open_subtree d ~sw path =
    let sub = call d path (fun () -> Eio.Path.open_subtree ~sw (at d path)) in
    let close () = Eio.Resource.close (fst sub) in
    (Eio.Resource.T
       ( { r = d.r; base = (sub :> Eio.Fs.dir_ty Eio.Path.t); close },
         Handler.closable )
      :> [ `Close | Eio.Fs.dir_ty ] Eio.Resource.t)

  let read_dir d path = call d path (fun () -> Eio.Path.read_dir (at d path))

  let with_dir_entries d path f =
    f
      (List.to_seq
         (call d path (fun () -> Eio.Path.read_dir_entries (at d path))))

  let stat d ~follow path =
    call d path (fun () -> Eio.Path.stat ~follow (at d path))

  let unlink d path = call d path (fun () -> Eio.Path.unlink (at d path))
  let rmdir d path = call d path (fun () -> Eio.Path.rmdir (at d path))

  let rename d path (Eio.Resource.T (t2, ops2) as dir2) path2 =
    let dest =
      match Eio.Resource.get_opt ops2 Base with
      | Some base -> Eio.Path.(base t2 / path2)
      | None -> ((dir2 :> Eio.Fs.dir_ty Eio.Resource.t), path2)
    in
    call d path (fun () -> Eio.Path.rename (at d path) dest)

  let read_link d path = call d path (fun () -> Eio.Path.read_link (at d path))

  let symlink ~link_to d path =
    call d path (fun () -> Eio.Path.symlink ~link_to (at d path))

  let chmod d ~follow ~perm path =
    call d path (fun () -> Eio.Path.chmod ~follow ~perm (at d path))

  let chown ~follow ?uid ?gid d path =
    call d path (fun () -> Eio.Path.chown ~follow ?uid ?gid (at d path))

  let sync_dir d path = call d path (fun () -> Eio.Path.sync_dir (at d path))
  let pp ppf d = Fmt.pf ppf "remote:%a" Eio.Path.pp d.base
  let native _ _ = None

  let split p =
    match String.rindex_opt p '/' with
    | None -> if p = "" then None else Some ("", p)
    | Some i ->
        Some (String.sub p 0 i, String.sub p (i + 1) (String.length p - i - 1))

  let join a b =
    if a = "" then b
    else if b = "" then a
    else if b.[0] = '/' then b
    else a ^ "/" ^ b
end

and Handler : sig
  val dir : (dir, [ `Remote | Eio.Fs.dir_ty ]) Eio.Resource.handler

  val closable :
    (dir, [ `Close | `Remote | Eio.Fs.dir_ty ]) Eio.Resource.handler
end = struct
  let base d = d.base

  let dir =
    Eio.Resource.handler [ H (Eio.Fs.Pi.Dir, (module Dir)); H (Base, base) ]

  let closable =
    Eio.Resource.handler
      [
        H (Eio.Fs.Pi.Dir, (module Dir));
        H (Base, base);
        H (Eio.Resource.Close, fun d -> d.close ());
      ]
end

let name env =
  let b = Cstruct.create 6 in
  Eio.Flow.read_exact env#secure_random b;
  "srv" ^ Cstruct.to_hex_string b

let v ?(leak = false) ~sw (env : Eio_chaos.env) =
  let base = Eio.Path.(env#fs / name env) in
  Eio.Path.mkdir ~perm:0o755 base;
  let t = { env; sw; leak; calls = Hashtbl.create 8; next = 0; conn = None } in
  let l = Eio.Net.listen ~reuse_addr:true ~backlog:4 ~sw env#net addr in
  Eio.Fiber.fork_daemon ~sw (fun () -> serve t l);
  ( (Eio.Resource.T ({ r = t; base; close = ignore }, Handler.dir)
      :> Eio.Fs.dir_ty Eio.Resource.t),
    "" )
