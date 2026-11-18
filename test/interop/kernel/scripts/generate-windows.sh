#!/bin/bash
# Records the outcomes through eio_windows, in a fresh directory of the
# temporary directory, on its NTFS volume: as the user running it, then as a
# basic user (basic-user.sh), who may not make symbolic links. The columns
# macOS and Linux recorded, generate.sh, are kept from the trace before.
# probe-windows.sh prints where it ran.
#   generate-windows.sh OUT_DIR RECORD_EXE
set -euo pipefail
case "$(uname -s)" in
MINGW* | MSYS* | CYGWIN*) ;;
*)
  echo "generate-windows.sh records Windows: run it on Windows" >&2
  exit 1
  ;;
esac
out="$(cd "$1" && pwd)"
record="$(cd "$(dirname "$2")" && pwd)/$(basename "$2")"
dir="$(cd "$(dirname "$0")" && pwd)"
src="${DUNE_SOURCEROOT:?run through dune: REGEN=1 dune build @regen-traces}"
work="$(mktemp -d "${TMPDIR:-/tmp}/fs-traces.XXXXXX")"
# A name Win32 cannot spell, such as NUL or a trailing dot, may stay behind.
trap 'rm -rf "$work" || true' EXIT
mkdir "$work/user" "$work/basic"
(cd "$work/user" && "$record" windows >"$work/user.csv")
# Without runas, the column says "not recorded", and the probe says why.
bash "$dir/basic-user.sh" "$work/basic" "$(cygpath -w "$record")" windows basic user \
  >"$work/basic.csv" || echo ops >"$work/basic.csv"
# The recorder is a Windows program: it takes Windows paths.
"$record" merge "$(cygpath -w "$src/test/interop/kernel/traces/fs.csv")" \
  "$(cygpath -w "$work/user.csv")" "$(cygpath -w "$work/basic.csv")" >"$out/fs.csv"
