#!/bin/bash
# Records the outcomes through eio_posix as a user: macOS natively, on the
# default case-insensitive APFS, so this runs on a Mac, and Linux in Docker,
# on a tmpfs, in the image pinned below. The trace starts with the "# key:
# value" lines test/os/README.md lists.
#   generate.sh OUT RECORD_EXE
set -euo pipefail
[ "$(uname -s)" = Darwin ] || {
  echo "generate.sh records macOS natively: run it on macOS" >&2
  exit 1
}
command -v docker >/dev/null || { echo "docker not on PATH" >&2; exit 1; }
out="$(cd "$(dirname "$1")" && pwd)/$(basename "$1")"
record="$(cd "$(dirname "$2")" && pwd)/$(basename "$2")"
src="${DUNE_SOURCEROOT:?run through dune: REGEN=1 dune build @regen-traces}"
image="ocaml/opam@sha256:d161f099e5d053fe198accc09fda214c6275289570847fd906f9c65f862bf460"

scripts=(test/os/scripts test/os/seq)
revision="$(git -C "$src" log -1 --format=%H -- "${scripts[@]}")"
[ -z "$(git -C "$src" status --porcelain -- "${scripts[@]}")" ] ||
  revision="$revision with uncommitted changes"
# test/os/test.ml checks this digest.
digest="$(cat "$src/test/os/scripts/record.ml" "$src/test/os/seq/os_seq.ml" | md5 -q)"

work="$(mktemp -d "${TMPDIR:-/tmp}/fs-traces.XXXXXX")"
trap 'chmod -R u+rwx "$work"; rm -rf "$work"' EXIT
mkdir "$work/macos" "$work/out" "$work/case"
(cd "$work/macos" && "$record" >"$work/out/macos.csv")

mp="$(df -P "$work/macos" | awk 'NR == 2 { print $6 }')"
mount_opts="$(mount | sed -n "s|^.* on $mp (\(.*\))\$|\1|p")"
touch "$work/case/a"
if [ -e "$work/case/A" ]; then case_=case-insensitive; else case_=case-sensitive; fi

# Only the recorder is built, from the two directories it needs.
docker run --rm --name fs-apfs-rec --user opam --group-add 4242 --tmpfs /work:exec,mode=1777 \
  -v "$src/test/os:/os:ro" -v "$work/out:/out" "$image" bash -euc "
    mkdir ~/rec && cp -r /os/seq /os/scripts ~/rec
    echo '(lang dune 3.0)' >~/rec/dune-project
    cd ~/rec
    opam update -y >/dev/null
    opam install -y eio_posix fmt cstruct optint >/dev/null
    opam exec -- dune build ./scripts/record.exe
    mkdir /work/linux /work/case && cd /work/linux
    ~/rec/_build/default/scripts/record.exe >/out/linux.csv
    touch /work/case/a
    if [ -e /work/case/A ]; then c=case-insensitive; else c=case-sensitive; fi
    os=\$(uname -srm)
    fs=\$(awk '\$2 == \"/work\" { print \$3 }' /proc/mounts)
    opts=\$(awk '\$2 == \"/work\" { print \$4 }' /proc/mounts)
    user=\"uid \$(id -u), groups \$(id -G)\"
    eio=\$(opam show -f installed-version eio_posix)
    printf '# linux %s: %s\\n' >/out/linux.meta \\
      os \"\$os\" image $image filesystem \"\$fs, \$c\" \\
      mount \"\$opts\" user \"\$user\" eio_posix \"\$eio\""

macos_os="$(sw_vers -productName) $(sw_vers -productVersion), $(uname -srm)"
macos_user="uid $(id -u), groups $(id -G)"
macos_eio="$(opam show -f installed-version eio_posix)"
{
  printf '# %s: %s\n' \
    script test/os/scripts/generate.sh \
    "script revision" "$revision" \
    "script digest" "$digest" \
    recorded "$(date -u +%Y-%m-%d)" \
    "macos os" "$macos_os" \
    "macos filesystem" "${mount_opts%%,*}, $case_" \
    "macos mount" "$mount_opts" \
    "macos user" "$macos_user" \
    "macos eio_posix" "$macos_eio"
  cat "$work/out/linux.meta"
  "$record" merge "$work/out/linux.csv" "$work/out/macos.csv"
} >"$out"
