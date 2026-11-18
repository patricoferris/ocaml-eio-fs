(** The Win32 paths [paths.exe] records through an unrestricted filesystem, and
    the in-memory filesystem's test replays. *)

val header : string
(** [header] is the CSV header of [traces/paths.csv]. *)

val rows :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  work:string ->
  ?other:string ->
  ?win32:(string -> string) ->
  unit ->
  string list
(** [rows ~fs ~work ~other ~win32 ()] runs each case through [fs], from the
    empty directory [work], the current one, with the empty directory [other] on
    another drive, and is a CSV row per case. [win32 p] is what a read of the
    Win32 path [p] answers outside Eio (defaults to ["-"]). *)
