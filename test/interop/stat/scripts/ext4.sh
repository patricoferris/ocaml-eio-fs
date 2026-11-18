#!/bin/bash
# Mounts at /work/ext4, for the user opam, a fresh ext4 image with 4096-byte
# blocks, mke2fs's choice for a disk rather than for a small image. Run in a
# --privileged container of the image generate.sh pins.
set -euo pipefail
sudo apt-get update -qq >/dev/null
sudo apt-get install -y -qq e2fsprogs >/dev/null
truncate -s 64M /tmp/ext4.img
/sbin/mkfs.ext4 -q -b 4096 /tmp/ext4.img
sudo mkdir /work/ext4
sudo mount -o loop /tmp/ext4.img /work/ext4
sudo chown opam /work/ext4
