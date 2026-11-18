#!/bin/bash
# Prints the letter of a fixed drive other than DRIVE, or nothing.
#   other-drive.sh DRIVE
set -euo pipefail
MSYS_NO_PATHCONV=1 powershell.exe -NoProfile -NonInteractive -Command \
  "Get-CimInstance Win32_LogicalDisk -Filter 'DriveType=3' | ForEach-Object { \$_.DeviceID.Substring(0,1) }" |
  tr -d '\r' | grep -v -i -x "$1" | head -n 1 || true
