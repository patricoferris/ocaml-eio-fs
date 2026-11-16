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

let base_dir (Eio.Resource.T (t, ops)) =
  Option.map (fun get -> get t) (Eio.Resource.get_opt ops Base_dir)

let base_file (Eio.Resource.T (t, ops)) =
  Option.map (fun get -> get t) (Eio.Resource.get_opt ops Base_file)

let admin (Eio.Resource.T (t, ops)) =
  Option.map (fun get -> get t) (Eio.Resource.get_opt ops Admin)

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
