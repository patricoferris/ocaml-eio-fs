(** The provenance a recorded trace starts with: its ["# key: value"] lines. *)

val check :
  trace:string -> keys:string list -> code:string list -> script:string -> unit
(** [check ~trace ~keys ~code ~script] fails unless the file [trace] has a
    measured line for each of [keys], its ["script digest"] is the MD5 of the
    files [code] joined in order, and its ["linux image"] is the image the
    script [script] pins. A value is measured unless empty, ["--"], as opam
    answers for a package the switch lacks, or starting with ["unknown"]. *)
