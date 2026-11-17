(** What the fault-injecting wrapper of {!Chaos_model.wrap} does with each
    operation of each interface it binds.

    With no fault, the wrapper answers as its base does, except where an entry
    below says otherwise. Each operation is in one class:
    - {!Forwarded}: the base answers, and no fault reaches it;
    - {!Intercepted}: the schedule is asked once, with the kind given, before
      the base runs, and a fault may refuse, shorten or follow the operation;
    - {!Synthesized}: the wrapper answers itself, from the base's other
      operations or from nothing;
    - {!Unsupported}: the wrapper binds no such interface, so it is refused with
      [Failure "Operation not supported!"], which [Eio.Resource.get] raises, and
      [Eio.Resource.get_opt] finds nothing.

    The operations of [Eio.Fs.Pi.DIR], [Eio.File.Pi.READ] and
    [Eio.File.Pi.WRITE] are listed in modules of those signatures, so an
    operation Eio adds is a type error here until it has an entry. *)

type kind =
  [ `Read
  | `Read_dir
  | `Write
  | `Truncate
  | `Sync
  | `Open_read
  | `Open_write
  | `Stat
  | `Change ]
(** The type for the operation kinds the schedule is asked about, those of
    {!Chaos_plan.Op.kind}. *)

(** The type for the classes of operations. *)
type cls =
  | Forwarded  (** The base answers. *)
  | Intercepted of kind  (** The schedule is asked first, with this kind. *)
  | Synthesized  (** The wrapper answers itself. *)
  | Unsupported  (** The wrapper binds no such interface. *)

(** The type for the interfaces operations belong to. *)
type iface =
  | Dir  (** [Eio.Fs.Pi.DIR], with its [PATH] operations. *)
  | Read  (** [Eio.File.Pi.READ], with [Eio.Flow.Pi.SOURCE]. *)
  | Write  (** [Eio.File.Pi.WRITE] beyond [READ], with [Eio.Flow.Pi.SINK]. *)
  | Dir_handler  (** The other interfaces a directory binds. *)
  | File_handler  (** The other interfaces a file binds. *)

type t = {
  iface : iface;  (** The interface the operation belongs to. *)
  name : string;  (** The operation's name in that interface. *)
  cls : cls;  (** What the wrapper does with it. *)
  why : string;  (** Why, in a sentence. *)
}
(** The type for entries of the law. *)

val all : t list
(** [all] is one entry for each operation, in interface order. *)

val get : iface -> string -> t
(** [get iface name] is the entry for [name] in [iface].

    @raise Not_found if there is none. *)

val binding :
  iface -> 'a Eio.Resource.binding -> [ `Interface of iface | `Op of t | `Other ]
(** [binding handler b] is what [b], a binding of a [handler], {!Dir_handler} or
    {!File_handler}, is: the interface whose operations {!all} lists, an
    operation of its own, or [`Other] for any other interface, which the wrapper
    does not carry over. An interface Eio does not export, such as
    [Eio.Net.Pi.Socket], is [`Other] too, and its entry in {!all} is named as
    Eio names it. *)

val pp_iface : iface Fmt.t
(** [pp_iface] formats an interface by its Eio name. *)

val pp_cls : cls Fmt.t
(** [pp_cls] formats a class. *)

val pp : t Fmt.t
(** [pp] formats an entry as its interface and its name. *)
