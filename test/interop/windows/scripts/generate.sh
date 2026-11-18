#!/bin/bash
# Records what Win32 paths through the unrestricted fs answer (paths.ml), in
# a fresh directory of the temporary directory, with a fresh directory on
# another fixed drive when there is one (other-drive.sh). probe.sh prints
# where it ran.
#   generate.sh OUT_DIR PATHS_EXE
set -euo pipefail
case "$(uname -s)" in
MINGW* | MSYS* | CYGWIN*) ;;
*)
  echo "generate.sh records Windows: run it on Windows" >&2
  exit 1
  ;;
esac
out="$(cd "$1" && pwd)"
paths="$(cd "$(dirname "$2")" && pwd)/$(basename "$2")"
dir="$(cd "$(dirname "$0")" && pwd)"
work="$(mktemp -d "${TMPDIR:-/tmp}/fs-traces.XXXXXX")"
other=""
# A name Win32 cannot spell, such as NUL or a trailing dot, may stay behind.
trap 'rm -rf "$work" ${other:+"$other"} || true' EXIT
mkdir "$work/paths"
other_drive="$(bash "$dir/other-drive.sh" "$(cygpath -w "$work" | cut -c1)")"
if [ -n "$other_drive" ]; then
  other="$(mktemp -d "$(cygpath -u "$other_drive:/")eio-fs-paths.XXXXXX")"
  (cd "$work/paths" && "$paths" "$(cygpath -w "$other")" >"$out/paths.csv")
else
  (cd "$work/paths" && "$paths" >"$out/paths.csv")
fi
