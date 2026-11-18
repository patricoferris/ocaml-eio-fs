open! Hermetic
module M = Chaos_model

type env = Eio_chaos.env
type Eio.Exn.Backend.t += Injected_fault = M.Injected_fault

module Op = Op
module Fault = Fault

(* {1 Wrapper} *)

let wrap f base =
  let hook ~space:_ op = f op in
  (M.wrap (M.schedule hook) base :> Eio.Fs.dir_ty Eio.Path.t)

(* {1 Power} *)

module Power = struct
  type change = M.Crash.unsynced

  let pp_change = M.Crash.pp_unsynced

  type policy = Lose_unsynced | Keep of (change -> bool)
  type t = M.Crash.t

  (* A controller built by hand may wrap a directory other processes write,
     so its cut keeps their changes. *)
  let v ?os = function
    | Lose_unsynced -> M.Crash.v ?os ~policy:Drop_unsynced ~outside:Kept ()
    | Keep p -> M.Crash.v ?os ~policy:(Survive p) ~outside:Kept ()

  let wrap t base = (M.Crash.wrap t base :> Eio.Fs.dir_ty Eio.Path.t)
  let unsynced = M.Crash.unsynced
  let cut = M.Crash.post_crash
end

(* {1 The check} *)

let name = "Eio_fs_chaos.run"
let disk = Disk.part

let run ?os ?budget ?seed ?replay ?setup ?inspect app =
  Eio_chaos.run ~name ?os ?budget ?seed ?replay ?setup ?inspect [ disk () ] app

let ack env file =
  let name = "Eio_fs_chaos.ack" in
  Ack.ack name (Disk.acks name env) file

let acked env = Ack.acked (Disk.acks "Eio_fs_chaos.acked" env)

(* The parts a device is built over, and a quiet disk unless one is. *)
let below under =
  if List.exists (fun p -> Eio_chaos.Private.name p = Disk.name) under then
    under
  else under @ [ Eio_chaos.quiet (disk ()) ]

let device ?os ?user ?(under = []) ?budget ?seed ?replay dir =
  let parts = below under in
  let run f = Eio_chaos.Private.within ?os parts f in
  if replay = None then Device.check ?os ?user ~run dir;
  if under <> [] then
    Eio_chaos.Private.run ~name:"Eio_fs_chaos.device" ~args:"dir"
      ~passes:[ Errors ] ?os ?budget ?seed ?replay parts
      (Device.program ?os ?user dir)

module Private = struct
  let check_faults ?budget ?seed ?replay ?os ?setup run =
    Eio_chaos.Private.run ~name ~passes:[ Errors ] ?budget ?seed ?replay ?os
      ?setup
      [ disk () ]
      run

  let check_cuts ?budget ?seed ?replay ?os ?setup run ~recover =
    let acked = ref [] in
    let app env =
      if Eio_chaos.Private.restarted env then
        recover ~acked:(List.rev !acked) env
      else begin
        acked := [];
        run
          ~ack:(fun v -> if Disk.acknowledge env then acked := v :: !acked)
          env
      end
    in
    Eio_chaos.Private.run ~name ~passes:[ Cuts ] ?budget ?seed ?replay ?os
      ?setup
      [ disk () ]
      app

  let scenarios = Device.scenarios
  let eio_posix_differs = Device.eio_posix_differs

  module Crash = M.Crash
  module Law = M.Law
  module Internal = M.Internal
end
