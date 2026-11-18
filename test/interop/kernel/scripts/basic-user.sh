#!/bin/bash
# Runs a cmd.exe command line in DIR as a basic user, with runas
# /trustlevel:0x20000: the user's own token without the Administrators group
# and the privileges it brings, SeCreateSymbolicLinkPrivilege among them.
# Prints the command's output once it is done. Paths must hold no space.
#   basic-user.sh DIR COMMAND...
set -euo pipefail
dir="$(cygpath -w "$1")"
shift
tmp="$(mktemp -d "${TMPDIR:-/tmp}/basic-user.XXXXXX")"
w="$(cygpath -w "$tmp")"
MSYS_NO_PATHCONV=1 runas /trustlevel:0x20000 \
  "cmd /c cd /d $dir && ($* >$w\\out 2>$w\\err && echo 0 >$w\\done || echo 1 >$w\\done)"
for _ in $(seq 900); do
  [ -s "$tmp/done" ] && break
  sleep 1
done
[ -s "$tmp/done" ] || { echo "basic-user.sh: $* never finished" >&2; exit 1; }
sleep 1
[ "$(tr -d '\r ' <"$tmp/done")" = 0 ] || {
  echo "basic-user.sh: $* failed: $(cat "$tmp/err")" >&2
  exit 1
}
cat "$tmp/out"
