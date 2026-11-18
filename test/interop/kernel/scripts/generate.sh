#!/bin/bash
# Records the outcomes through eio_posix as a user: macOS natively, on the
# default case-insensitive APFS, so this runs on a Mac, and Linux in Docker,
# on a tmpfs, in the image pinned below. Both run the eio_posix of the opam
# switch dune builds in. probe.sh prints where they ran. The columns
# Windows recorded, generate-windows.sh, are kept from the trace before.
#   generate.sh OUT_DIR RECORD_EXE
set -euo pipefail
[ "$(uname -s)" = Darwin ] || {
  echo "generate.sh records macOS natively: run it on macOS" >&2
  exit 1
}
command -v docker >/dev/null || { echo "docker not on PATH" >&2; exit 1; }
out="$(cd "$1" && pwd)"
record="$(cd "$(dirname "$2")" && pwd)/$(basename "$2")"
src="${DUNE_SOURCEROOT:?run through dune: REGEN=1 dune build @regen-traces}"
# The switch dune builds the recorder in; Linux installs the same eio_posix.
switch="${OPAM_SWITCH_PREFIX:?run in an opam switch}"
pkg=("$switch"/.opam-switch/packages/eio_posix.*)
[ -d "${pkg[0]}" ] || { echo "eio_posix is not installed in $switch" >&2; exit 1; }
eio="${pkg[0]##*/eio_posix.}"
# A switch that pins Eio to a source: Linux pins the same one.
pin="$(sed -n 's/^ *src: "\(.*\)"$/\1/p' "$switch/.opam-switch/overlay/eio_posix/opam" 2>/dev/null || true)"
image="ocaml/opam@sha256:d161f099e5d053fe198accc09fda214c6275289570847fd906f9c65f862bf460"

work="$(mktemp -d "${TMPDIR:-/tmp}/fs-traces.XXXXXX")"
trap 'chmod -R u+rwx "$work"; rm -rf "$work"' EXIT
mkdir "$work/macos" "$work/out"
(cd "$work/macos" && "$record" macos >"$work/out/macos.csv")

# Only the recorder is built, from the interpreter and its own directory.
docker run --rm --name fs-apfs-rec --user opam --group-add 4242 --tmpfs /work:exec,mode=1777 \
  -v "$src/test/interop/kernel/scripts:/os/scripts:ro" -v "$src/lib/kernel:/os/kernel:ro" \
  -v "$work/out:/out" "$image" bash -euc "
    mkdir -p ~/rec/kernel && cp -r /os/scripts ~/rec
    cp /os/kernel/eio_fs_kernel.ml /os/kernel/eio_fs_kernel.mli ~/rec/kernel
    echo '(library (name eio_fs_kernel) (libraries eio fmt cstruct optint))' >~/rec/kernel/dune
    echo '(lang dune 3.0)' >~/rec/dune-project
    cd ~/rec
    opam update -y >/dev/null
    if [ -n '$pin' ]; then opam pin add -yn eio.$eio '$pin' >/dev/null; opam pin add -yn eio_posix.$eio '$pin' >/dev/null; fi
    opam install -y eio_posix.$eio fmt cstruct optint >/dev/null
    opam exec -- dune build ./scripts/record.exe
    mkdir /work/linux && cd /work/linux
    ~/rec/_build/default/scripts/record.exe linux >/out/linux.csv"

"$record" merge "$src/test/interop/kernel/traces/fs.csv" \
  "$work/out/linux.csv" "$work/out/macos.csv" >"$out/fs.csv"
