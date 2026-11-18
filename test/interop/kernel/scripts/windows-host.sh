#!/bin/bash
# Prints, as "key: value" lines, where a Windows recording runs: the
# eio_windows of the switch and the source it is pinned to, the Windows it
# runs on, the filesystem of the temporary directory and whether a name's
# case matters there, whether Developer Mode is on, and whether the user, and
# the basic user basic-user.sh runs as, hold the privilege to make symbolic
# links.
set -euo pipefail
dir="$(dirname "$0")"
switch="${OPAM_SWITCH_PREFIX:?run in an opam switch}"
switch="$(cygpath -u "$switch")"
pkg=("$switch"/.opam-switch/packages/eio_windows.*)
[ -d "${pkg[0]}" ] || { echo "eio_windows is not installed in $switch" >&2; exit 1; }
pin="$(sed -n 's/^ *src: "\(.*\)"$/\1/p' "$switch/.opam-switch/overlay/eio_windows/opam" 2>/dev/null || true)"
echo "eio_windows: ${pkg[0]##*/eio_windows.}${pin:+ from $pin}"

ps() { MSYS_NO_PATHCONV=1 powershell.exe -NoProfile -NonInteractive -Command "$1" | tr -d '\r'; }
os() { ps "(Get-CimInstance Win32_OperatingSystem).$1"; }
link() { awk '/SeCreateSymbolicLinkPrivilege/ { print $NF; found = 1 } END { if (!found) print "absent" }'; }
work="$(mktemp -d "${TMPDIR:-/tmp}/fs-probe.XXXXXX")"
trap 'rm -rf "$work"' EXIT
touch "$work/a"
if [ -e "$work/A" ]; then case_=case-insensitive; else case_=case-sensitive; fi
drive="$(cygpath -w "$work" | cut -c1)"
devmode="$(ps '(Get-ItemProperty -Path HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\AppModelUnlock -ErrorAction SilentlyContinue).AllowDevelopmentWithoutDevLicense')"
admin="$(ps '([Security.Principal.WindowsPrincipal][Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)')"
echo "windows: $(os Caption) $(os Version)"
echo "windows.image: ${ImageOS:-unknown} ${ImageVersion:-}"
echo "windows.filesystem: $(ps "(Get-Volume -DriveLetter $drive).FileSystemType"), $case_"
if [ "$devmode" = 1 ]; then devmode=on; else devmode=off; fi
echo "windows.developer-mode: $devmode"
# Cygwin's whoami comes first on PATH.
whoami="$(cygpath -W)/System32/whoami.exe"
privs() { MSYS_NO_PATHCONV=1 "$whoami" /priv | tr -d '\r' | link; }
if basic="$(bash "$dir/basic-user.sh" "$work" "$(cygpath -w "$whoami")" /priv)"; then
  basic="SeCreateSymbolicLinkPrivilege $(tr -d '\r' <<<"$basic" | link)"
else
  basic="unavailable: runas /trustlevel failed"
fi
echo "windows.user: administrator $admin, SeCreateSymbolicLinkPrivilege $(privs)"
echo "windows.basic-user: $basic"
