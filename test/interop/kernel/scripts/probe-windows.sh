#!/bin/bash
# Prints, as "key: value" lines, where generate-windows.sh records
# (windows-host.sh), and, kept from the provenance before, where the macOS
# and Linux columns were recorded.
set -euo pipefail
dir="$(dirname "$0")"
bash "$dir/windows-host.sh"
bash "$dir/carry.sh" "${DUNE_SOURCEROOT:?run through dune}/test/interop/kernel/traces/provenance" \
  posix Darwin "eio_posix: |macos[.:]|linux[.:]|docker: "
