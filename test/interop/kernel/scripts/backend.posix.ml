let run f =
  Eio_posix.run @@ fun env -> f (Eio.Stdenv.cwd env :> Eio.Fs.dir_ty Eio.Path.t)
