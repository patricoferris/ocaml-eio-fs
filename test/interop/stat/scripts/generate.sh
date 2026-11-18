#!/bin/bash
# Records the stat answers of test/interop/stat/workload through eio_posix on
# Linux, in Docker, in the image pinned below: on a tmpfs, and on the ext4
# image ext4.sh mounts, which takes --privileged. Linux runs the eio_posix of
# the opam switch dune builds in. probe.sh prints where it ran.
#   generate.sh OUT_DIR
set -euo pipefail
command -v docker >/dev/null || { echo "docker not on PATH" >&2; exit 1; }
out="$(cd "$1" && pwd)"
src="${DUNE_SOURCEROOT:?run through dune: REGEN=1 dune build @regen-traces}"
# The switch dune builds the recorder in; Linux installs the same eio_posix.
switch="${OPAM_SWITCH_PREFIX:?run in an opam switch}"
pkg=("$switch"/.opam-switch/packages/eio_posix.*)
[ -d "${pkg[0]}" ] || { echo "eio_posix is not installed in $switch" >&2; exit 1; }
eio="${pkg[0]##*/eio_posix.}"
# A switch that pins Eio to a source: Linux pins the same one.
pin="$(sed -n 's/^ *src: "\(.*\)"$/\1/p' "$switch/.opam-switch/overlay/eio_posix/opam" 2>/dev/null || true)"
image="ocaml/opam@sha256:d161f099e5d053fe198accc09fda214c6275289570847fd906f9c65f862bf460"

work="$(mktemp -d "${TMPDIR:-/tmp}/stat-traces.XXXXXX")"
trap 'rm -rf "$work"' EXIT

# Only the recorder is built, from the two directories it needs.
docker run --rm --name fs-stat-rec --privileged --user opam \
  --tmpfs /work/tmpfs:exec,mode=1777 \
  -v "$src/test/interop/stat:/stat:ro" -v "$work:/out" "$image" bash -euc "
    bash /stat/scripts/ext4.sh
    mkdir ~/rec && cp -r /stat/workload /stat/scripts ~/rec
    echo '(lang dune 3.0)' >~/rec/dune-project
    cd ~/rec
    opam update -y >/dev/null
    if [ -n '$pin' ]; then opam pin add -yn eio.$eio '$pin' >/dev/null; opam pin add -yn eio_posix.$eio '$pin' >/dev/null; fi
    opam install -y eio_posix.$eio fmt cstruct optint >/dev/null
    opam exec -- dune build ./scripts/record.exe
    ./_build/default/scripts/record.exe tmpfs=/work/tmpfs ext4=/work/ext4 \\
      >/out/linux.csv
    sudo umount /work/ext4"
cp "$work/linux.csv" "$out/linux.csv"
