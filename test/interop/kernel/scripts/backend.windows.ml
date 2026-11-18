let run f =
  Eio_windows.run @@ fun env ->
  f (Eio.Stdenv.cwd env :> Eio.Fs.dir_ty Eio.Path.t)
