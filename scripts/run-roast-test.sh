#!/bin/bash
# Wrapper for running a single roast test with a per-file timeout.
# Used by `make roast` via: prove -e 'MUTSU_BIN=... scripts/run-roast-test.sh'
#
# Usage: scripts/run-roast-test.sh <test-file>

MUTSU_BIN="${MUTSU_BIN:-target/release/mutsu}"
DEFAULT_TIMEOUT=30

per_file_timeout() {
  local test_file="$1"
  case "$test_file" in
    roast/S17-supply/batch.t)
      # This test uses sleep 5 in after-tap blocks (4 time-based subtests x2 schedulers).
      # Even raku takes ~37s to run it. CI can be slower.
      echo 120
      ;;
    roast/S17-supply/unique.t)
      # This test intentionally uses multiple sleep 1 calls and needs >30s.
      echo 60
      ;;
    roast/S17-promise/allof.t)
      # This test uses sleep 2*$_ with $_ up to 9, parallel start blocks take ~18s.
      echo 60
      ;;
    *)
      echo "$DEFAULT_TIMEOUT"
      ;;
  esac
}

test_file="$1"
file_timeout=$(per_file_timeout "$test_file")
exec timeout "$file_timeout" "$MUTSU_BIN" "$test_file"
