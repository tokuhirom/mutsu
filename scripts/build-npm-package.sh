#!/usr/bin/env bash
# Build the browser package that is published to npm as `mutsu`.
set -euo pipefail

wasm-pack build --target web --no-default-features --features wasm
cp site/assets/embed.js pkg/embed.js
cp docs/browser-embedding.md pkg/README.md

# wasm-pack supplies mutsu.js, mutsu.d.ts, mutsu_bg.wasm, package.json, and the
# license files. Keep the hand-written Web Component and guide beside them.
node scripts/prepare-npm-package.mjs pkg/package.json
