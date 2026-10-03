#!/usr/bin/env bash
# Install a pinned, checksum-verified wasm-pack and wasm-opt into ~/.cargo/bin.
#
# Replaces `curl https://rustwasm.github.io/wasm-pack/installer/init.sh | sh`,
# which ran an unpinned script from a site of an archived organisation (the
# project moved to wasm-bindgen/wasm-pack) inside the npm-publishing job.
#
# wasm-opt comes from binaryen directly because wasm-pack (0.13.1 and 0.15.0
# alike) would otherwise download binaryen version_117, whose `-O` pass took
# ~15 min on mutsu's ~34 MB module on a 4-core box; version_129 does the same
# `-O` in ~2.5 min and its output is slightly smaller. wasm-pack runs the
# `wasm-opt` it finds on PATH (with the same `-O`) before downloading its own,
# so installing it next to wasm-pack is all that is needed.
#
# To bump either tool: download the new tarball, compare its SHA-256 with the
# digest GitHub shows on the release page (binaryen also publishes a
# `.tar.gz.sha256` next to each tarball), and update the values below.
set -euo pipefail

wasm_pack_version=0.15.0
wasm_pack_sha256=c09f971ecaed9a2efc80fdcea7a00ef6b53c7fadc8c57d1f61b53a6aa66b668a
binaryen_version=129
binaryen_sha256=50b9fa62b9abea752da92ec57e0c555fee578760cd237c40107957715d2976ba

work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
mkdir -p "$HOME/.cargo/bin"

# fetch <url> <sha256> <out>
fetch() {
  curl -sSfL --retry 3 -o "$3" "$1"
  echo "$2  $3" | sha256sum -c -
}

name="wasm-pack-v${wasm_pack_version}-x86_64-unknown-linux-musl"
fetch "https://github.com/wasm-bindgen/wasm-pack/releases/download/v${wasm_pack_version}/${name}.tar.gz" \
  "$wasm_pack_sha256" "$work/wasm-pack.tar.gz"
tar -xzf "$work/wasm-pack.tar.gz" -C "$work"
install -m 0755 "$work/$name/wasm-pack" "$HOME/.cargo/bin/wasm-pack"
"$HOME/.cargo/bin/wasm-pack" --version

name="binaryen-version_${binaryen_version}"
fetch "https://github.com/WebAssembly/binaryen/releases/download/version_${binaryen_version}/${name}-x86_64-linux.tar.gz" \
  "$binaryen_sha256" "$work/binaryen.tar.gz"
tar -xzf "$work/binaryen.tar.gz" -C "$work" "$name/bin/wasm-opt"
install -m 0755 "$work/$name/bin/wasm-opt" "$HOME/.cargo/bin/wasm-opt"
"$HOME/.cargo/bin/wasm-opt" --version
