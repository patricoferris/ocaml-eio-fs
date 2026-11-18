#!/bin/bash
# Prints, as "key: value" lines, where generate.sh records: the eio_posix of
# the switch, the Mac it runs on with the filesystem of $TMPDIR, and a
# container of the image generate.sh pins, run as generate.sh runs one, on a
# tmpfs, as an ordinary user with a supplementary group.
set -euo pipefail
dir="$(dirname "$0")"
image="$(sed -n 's/^image="\(.*\)"$/\1/p' "$dir/generate.sh")"
[ -n "$image" ] || { echo "probe.sh: no image in generate.sh" >&2; exit 1; }
switch="${OPAM_SWITCH_PREFIX:?run in an opam switch}"
pkg=("$switch"/.opam-switch/packages/eio_posix.*)
pin="$(sed -n 's/^ *src: "\(.*\)"$/\1/p' "$switch/.opam-switch/overlay/eio_posix/opam" 2>/dev/null || true)"
echo "eio_posix: ${pkg[0]##*/eio_posix.}${pin:+ from $pin}"

work="$(mktemp -d "${TMPDIR:-/tmp}/fs-probe.XXXXXX")"
trap 'rm -rf "$work"' EXIT
touch "$work/a"
if [ -e "$work/A" ]; then case_=case-insensitive; else case_=case-sensitive; fi
mp="$(df -P "$work" | awk 'NR == 2 { print $6 }')"
mount_opts="$(mount | sed -n "s|^.* on $mp (\(.*\))\$|\1|p")"
echo "macos: $(sw_vers -productVersion) ($(sw_vers -buildVersion))"
echo "macos.filesystem: ${mount_opts%%,*}, $case_"
echo "macos.mount: $mount_opts"
echo "macos.user: uid $(id -u), groups $(id -G)"

echo "docker: $(docker version --format '{{.Server.Version}}')"
echo "linux.image: $image"
docker run --rm --network none --user opam --group-add 4242 \
  --tmpfs /work:exec,mode=1777 "$image" bash -euc '
  touch /work/a
  if [ -e /work/A ]; then c=case-insensitive; else c=case-sensitive; fi
  echo "linux: $(uname -srvm)"
  echo "linux.filesystem: $(awk -v m=/work "\$2 == m { print \$3 }" /proc/mounts), $c"
  echo "linux.mount: $(awk -v m=/work "\$2 == m { print \$4 }" /proc/mounts)"
  echo "linux.user: uid $(id -u), groups $(id -G)"'
