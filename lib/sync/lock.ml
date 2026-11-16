open! Hermetic

(* The field quantifies the section's result, so one lock serves sections of
   every result type. *)
type t = { run : 'a. (unit -> 'a) -> 'a }

(* A runner that cannot enter raises this in place of its lock's own
   exceptions, which a backend-free component cannot name. *)
exception Unavailable of string

let () =
  Printexc.register_printer (function
    | Unavailable why -> Some ("Eio_fs_sync.Lock.Unavailable: " ^ why)
    | _ -> None)

let use t f = t.run f
let none = { run = (fun f -> f ()) }

module type S = sig
  type t

  val use : t -> (unit -> 'a) -> 'a
end

(* Eta-expanded so that [L.use] checks against the quantified field. *)
module Make (L : S) = struct
  let v l = { run = (fun f -> L.use l f) }
end
