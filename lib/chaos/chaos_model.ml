module Crash = Crash
module Law = Law

module Internal = struct
  let replay_copied () = Atomic.get Blocks.copied
  let crash_image = Crash.image
  let descriptors = Memfs.descriptors
end
