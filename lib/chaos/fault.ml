open! Hermetic

type code = [ `Enoent | `Eacces | `Eperm | `Erofs | `Enospc | `Eio ]

type t =
  | Fail of code
  | Evict of code
  | Short of int
  | Torn of int
  | Swap_symlink of string

let code_name : code -> string = function
  | `Enoent -> "ENOENT"
  | `Eacces -> "EACCES"
  | `Eperm -> "EPERM"
  | `Erofs -> "EROFS"
  | `Enospc -> "ENOSPC"
  | `Eio -> "EIO"

let pp ppf = function
  | Fail c -> Fmt.pf ppf "fails with %s" (code_name c)
  | Evict c -> Fmt.pf ppf "fails with %s, its pages evicted" (code_name c)
  | Short n ->
      let n = max 1 n in
      Fmt.pf ppf "moves %d byte%s" n (if n = 1 then "" else "s")
  | Torn n -> Fmt.pf ppf "is torn after %d byte%s" n (if n = 1 then "" else "s")
  | Swap_symlink t -> Fmt.pf ppf "is swapped for a link to %s" t

let key = function
  | Fail c -> String.lowercase_ascii (code_name c)
  | Evict c -> "evict-" ^ String.lowercase_ascii (code_name c)
  | Short n -> Fmt.str "short%d" n
  | Torn n -> Fmt.str "torn%d" n
  | Swap_symlink t -> "swap:" ^ t
