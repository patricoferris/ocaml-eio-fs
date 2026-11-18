#!/bin/bash
# Prints, as "key: value" lines, where generate.sh records: the eio_posix of
# the switch, and a container of the image generate.sh pins, run as
# generate.sh runs one, with its tmpfs and the ext4 image ext4.sh mounts.
set -euo pipefail
dir="$(cd "$(dirname "$0")" && pwd)"
image="$(sed -n 's/^image="\(.*\)"$/\1/p' "$dir/generate.sh")"
[ -n "$image" ] || { echo "probe.sh: no image in generate.sh" >&2; exit 1; }
switch="${OPAM_SWITCH_PREFIX:?run in an opam switch}"
pkg=("$switch"/.opam-switch/packages/eio_posix.*)
echo "eio_posix: ${pkg[0]##*/eio_posix.}"

echo "docker: $(docker version --format '{{.Server.Version}}')"
echo "linux.image: $image"
docker run --rm --privileged --user opam --tmpfs /work/tmpfs:exec,mode=1777 \
  -v "$dir:/scripts:ro" "$image" bash -euc '
  bash /scripts/ext4.sh
  mnt() { awk -v m="$1" "\$2 == m { print \$3 \", \" \$4 }" /proc/mounts; }
  echo "linux: $(uname -srvm)"
  echo "linux.user: uid $(id -u), groups $(id -G)"
  echo "linux.tmpfs: $(mnt /work/tmpfs)"
  mkfs=$(/sbin/mkfs.ext4 -V 2>&1 | head -1)
  bs=$(/sbin/tune2fs -l /tmp/ext4.img | awk -F": *" "/^Block size/ { print \$2 }")
  echo "linux.ext4: $(mnt /work/ext4), $mkfs, $bs-byte blocks"
  sudo umount /work/ext4'
