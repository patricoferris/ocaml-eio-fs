#!/bin/bash
# Records the stat answers of test/stat/workload through eio_posix on Linux, in
# Docker, in the image pinned below: on a tmpfs, and on an ext4 image the
# container loop-mounts, which takes --privileged. The trace starts with the
# "# key: value" lines test/stat/README.md lists.
#   generate.sh OUT
set -euo pipefail
command -v docker >/dev/null || { echo "docker not on PATH" >&2; exit 1; }
out="$(cd "$(dirname "$1")" && pwd)/$(basename "$1")"
src="${DUNE_SOURCEROOT:?run through dune: REGEN=1 dune build @regen-traces}"
# The switch dune builds the recorder in; Linux installs the same eio_posix.
switch="${OPAM_SWITCH_PREFIX:?run in an opam switch}"
pkg=("$switch"/.opam-switch/packages/eio_posix.*)
[ -d "${pkg[0]}" ] || { echo "eio_posix is not installed in $switch" >&2; exit 1; }
eio="${pkg[0]##*/eio_posix.}"
image="ocaml/opam@sha256:d161f099e5d053fe198accc09fda214c6275289570847fd906f9c65f862bf460"

scripts=(test/stat/scripts test/stat/workload)
revision="$(git -C "$src" log -1 --format=%H -- "${scripts[@]}")"
[ -z "$(git -C "$src" status --porcelain -- "${scripts[@]}")" ] ||
  revision="$revision with uncommitted changes"

work="$(mktemp -d "${TMPDIR:-/tmp}/stat-traces.XXXXXX")"
trap 'rm -rf "$work"' EXIT

# Only the recorder is built, from the two directories it needs.
docker run --rm --name fs-stat-rec --privileged --user opam \
  --tmpfs /work/tmpfs:exec,mode=1777 \
  -v "$src/test/stat:/stat:ro" -v "$work:/out" "$image" bash -euc "
    sudo apt-get update -qq >/dev/null
    sudo apt-get install -y -qq e2fsprogs >/dev/null
    truncate -s 64M /tmp/ext4.img
    /sbin/mkfs.ext4 -q /tmp/ext4.img
    sudo mkdir /work/ext4
    sudo mount -o loop /tmp/ext4.img /work/ext4
    sudo chown opam /work/ext4
    mkdir ~/rec && cp -r /stat/workload /stat/scripts ~/rec
    echo '(lang dune 3.0)' >~/rec/dune-project
    cd ~/rec
    opam update -y >/dev/null
    opam install -y eio_posix.$eio fmt cstruct optint >/dev/null
    opam exec -- dune build ./scripts/record.exe
    ./_build/default/scripts/record.exe tmpfs=/work/tmpfs ext4=/work/ext4 \\
      >/out/linux.csv
    # test/test_memfs.ml checks this digest.
    digest=\$(cat scripts/record.ml workload/stat_workload.ml | md5sum)
    mnt() { awk -v m=\"\$1\" '\$2 == m { print \$3 \", \" \$4 }' /proc/mounts; }
    mkfs=\$(/sbin/mkfs.ext4 -V 2>&1 | head -1)
    bs=\$(/sbin/tune2fs -l /tmp/ext4.img | awk -F': *' '/^Block size/ { print \$2 }')
    printf '# %s: %s\\n' >/out/linux.meta \\
      'script digest' \"\${digest%% *}\" \\
      'linux os' \"\$(uname -srm)\" \\
      'linux image' $image \\
      'linux user' \"uid \$(id -u), groups \$(id -G)\" \\
      'linux eio_posix' \"\$(opam show -f installed-version eio_posix)\" \\
      tmpfs \"\$(mnt /work/tmpfs)\" \\
      ext4 \"\$(mnt /work/ext4), \$mkfs defaults, \$bs-byte blocks\"
    sudo umount /work/ext4"

{
  printf '# %s: %s\n' \
    script test/stat/scripts/generate.sh \
    "script revision" "$revision" \
    recorded "$(date -u +%Y-%m-%d)"
  cat "$work/linux.meta" "$work/linux.csv"
} >"$out"
