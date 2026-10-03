#!/usr/bin/env bash
# Install a pinned, checksum-verified wasm-pack into ~/.cargo/bin.
#
# Replaces `curl https://rustwasm.github.io/wasm-pack/installer/init.sh | sh`,
# which ran an unpinned script from a site of an archived organisation (the
# project moved to wasm-bindgen/wasm-pack) inside the npm-publishing job.
# That installer resolved to v0.13.1; this keeps the same version.
#
# To bump: download the new x86_64-unknown-linux-musl tarball, compare its
# SHA-256 with the digest GitHub shows on the release page, and update both
# values below.
set -euo pipefail

version=0.13.1
sha256=c539d91ccab2591a7e975bcf82c82e1911b03335c80aa83d67ad25ed2ad06539
name="wasm-pack-v${version}-x86_64-unknown-linux-musl"
url="https://github.com/wasm-bindgen/wasm-pack/releases/download/v${version}/${name}.tar.gz"

work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
curl -sSfL --retry 3 -o "$work/wasm-pack.tar.gz" "$url"
echo "${sha256}  $work/wasm-pack.tar.gz" | sha256sum -c -
tar -xzf "$work/wasm-pack.tar.gz" -C "$work"
mkdir -p "$HOME/.cargo/bin"
install -m 0755 "$work/$name/wasm-pack" "$HOME/.cargo/bin/wasm-pack"
"$HOME/.cargo/bin/wasm-pack" --version
