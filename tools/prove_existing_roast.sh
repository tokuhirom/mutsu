#!/usr/bin/env sh
set -eu

cargo build

roast_tests=$(rg -o --no-filename -e '"\.\./roast/[^"]+"' tests/*.rs | tr -d '"' | sort -u)
if [ -z "$roast_tests" ]; then
  echo "no roast tests found in tests/*.rs" >&2
  exit 2
fi

prove -e ./target/debug/mutsu $roast_tests
