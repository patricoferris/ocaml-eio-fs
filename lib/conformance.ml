open! Hermetic

let scenarios = List.map fst Eio_fs_kernel.scenarios
let check ?os:_ ?only:_ ~user:_ _ = failwith "Eio_fs.Conformance.check"
let eio_posix_differs _ = failwith "Eio_fs.Conformance.eio_posix_differs"
