open! Hermetic

type entry = { path : string; held : string; at : int }

type t = {
  base : Eio.Fs.dir_ty Eio.Path.t;
  at : unit -> int;
  alive : unit -> bool;
  mutable on_ack : unit -> unit;
  mutable live : entry list; (* latest first *)
  mutable paths : string list; (* latest first, each once *)
}

let v ~at ~alive base =
  {
    base :> Eio.Fs.dir_ty Eio.Path.t;
    at;
    alive;
    on_ack = ignore;
    live = [];
    paths = [];
  }

let on_ack t f = t.on_ack <- f

(* A path as the root names it, with no [""], ["."] or [".."]. *)
let norm path =
  let rec go acc = function
    | [] -> List.rev acc
    | ("" | ".") :: l -> go acc l
    | ".." :: l -> go (match acc with [] -> [] | _ :: a -> a) l
    | c :: l -> go (c :: acc) l
  in
  String.concat "/" (go [] (String.split_on_char '/' path))

let read t p =
  let f = Eio.Path.(t.base / p) in
  match Eio.Path.kind ~follow:false f with
  | `Regular_file -> Some (Eio.Path.load f)
  | _ -> None
  | exception Eio.Io _ -> None

let add name t ((_, path) : _ Eio.Path.t) =
  if t.alive () then
    let p = norm path in
    match read t p with
    | None ->
        Fmt.invalid_arg "%s: %S is not a regular file of the run's filesystem"
          name path
    | Some held ->
        t.live <-
          { path = p; held; at = t.at () }
          :: List.filter (fun e -> e.path <> p) t.live;
        if not (List.mem p t.paths) then t.paths <- p :: t.paths;
        t.on_ack ()

let acked t = List.rev_map (fun p -> "/" ^ p) t.paths

let under ~dir p =
  dir = "" || p = dir || String.starts_with ~prefix:(dir ^ "/") p

let touches (op : Op.t) p =
  match op.kind with
  | `Open_write | `Write | `Truncate -> norm op.path = p
  | `Change ->
      under ~dir:(norm op.path) p
      || Option.fold ~none:false ~some:(fun d -> under ~dir:(norm d) p) op.dest
  | `Open_read | `Stat | `Read | `Read_dir | `Sync -> false

let touch t op =
  if t.live <> [] then
    t.live <- List.filter (fun e -> not (touches op e.path)) t.live

let quote s =
  let n = String.length s in
  if n <= 40 then Fmt.str "%S" s
  else Fmt.str "%S... (%d bytes)" (String.sub s 0 32) n

(* The first file acknowledged and untouched since that no longer holds
   what it held then. *)
let lost t =
  List.find_map
    (fun e ->
      match read t e.path with
      | Some s when s = e.held -> None
      | now ->
          Fmt.kstr Option.some "/%s held %s when acknowledged %s, and now %s"
            e.path (quote e.held)
            (if e.at = 0 then "at the start" else Fmt.str "after #%d" e.at)
            (match now with
            | None -> "is missing"
            | Some s -> "holds " ^ quote s))
    (List.rev t.live)
