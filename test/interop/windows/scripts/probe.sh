#!/bin/bash
# Prints, as "key: value" lines, where generate.sh records: the Windows host
# (../../kernel/scripts/windows-host.sh) and the other fixed drive paths.csv
# renames to.
set -euo pipefail
dir="$(dirname "$0")"
bash "$dir/../../kernel/scripts/windows-host.sh"
work="$(mktemp -d "${TMPDIR:-/tmp}/fs-probe.XXXXXX")"
trap 'rm -rf "$work"' EXIT
drive="$(cygpath -w "$work" | cut -c1)"
other="$(bash "$dir/other-drive.sh" "$drive")"
if [ -n "$other" ]; then
  fs="$(MSYS_NO_PATHCONV=1 powershell.exe -NoProfile -NonInteractive -Command \
    "(Get-Volume -DriveLetter $other).FileSystemType" | tr -d '\r')"
  echo "windows.other-drive: $other:, $fs"
else
  echo "windows.other-drive: none"
fi
