open! Hermetic

type path = Eio.Fs.dir_ty Eio.Path.t
type t = path -> path -> unit

module type S = sig
  type t

  val rename : t -> path -> path -> unit
end

module Make (B : S) = struct
  let v backend = B.rename backend
end

let capability source _ =
  Fmt.invalid_arg
    "Eio_fs.Sync.Rename.exclusive: source is %a, in a directory with no \
     exclusive rename, pass a backend such as Eio_fs_unix.exclusive_rename"
    Eio.Path.pp source

type (_, _, _) Eio.Resource.pi +=
  | Exclusive :
      ( 'a,
        'a -> t -> source:string -> target:path -> unit,
        [> `Exclusive_rename ] )
      Eio.Resource.pi

let exclusive backend ((res, source) as old_path) target =
  let target = (target :> path) in
  let (Eio.Resource.T (v, ops)) = res in
  match Eio.Resource.get_opt ops Exclusive with
  | Some rename -> rename v backend ~source ~target
  | None -> backend (old_path :> path) target
