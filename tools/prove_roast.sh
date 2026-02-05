#!/usr/bin/env sh
set -eu

cargo build

if [ "$#" -eq 0 ]; then
  echo "usage: tools/prove_roast.sh <roast-test-or-dir> [...]" >&2
  exit 2
fi

prove -e ./target/debug/mutsu "$@"
