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
      # Even raku takes ~37s to run it. CI runners are ~3x slower.
      echo 180
      ;;
    roast/S17-supply/unique.t)
      # This test intentionally uses multiple sleep 1 calls and needs >30s.
      echo 60
      ;;
    roast/S17-promise/allof.t)
      # This test uses sleep 2*$_ with $_ up to 9, parallel start blocks take ~18s.
      echo 60
      ;;
    roast/S29-context/sleep.t)
      # This test has four 3-second sleep calls plus a subprocess, totalling ~18s locally
      # and potentially more on slower CI machines.
      echo 60
      ;;
    roast/S32-io/IO-Socket-Async.t)
      # Multiple TCP socket connection tests with real network I/O.
      # Tests 10+ use encoding features that may be slower on CI.
      echo 120
      ;;
    roast/S06-signature/named-parameters.t)
      # This test has a 1M-iteration hot loop (test 100) that takes ~8s on release builds.
      echo 60
      ;;
    roast/S03-sequence/exhaustive.t)
      # This test runs 124 subtests covering many sequence variants including
      # infinite sequences with head() truncation. Release builds take ~15s.
      echo 60
      ;;
    *)
      echo "$DEFAULT_TIMEOUT"
      ;;
  esac
}

# Some roast tests locate their data files relative to the spec root using a
# pattern like `"S32-str".IO.d ?? "S32-str" !! "t/spec/S32-str"`. In the
# upstream roast harness the CWD is the spec root, so `"S32-str".IO.d` is true.
# In our vendored layout the spec root is `roast/`, so these tests must run
# with CWD set to `roast/` for the relative data paths (e.g. text-samples/) to
# resolve. This does not affect tests that locate resources via `$*PROGRAM`.
needs_roast_cwd() {
  case "$1" in
    roast/S32-str/gb18030-encode-decode.t \
    | roast/S32-str/gb2312-encode-decode.t \
    | roast/S32-str/shiftjis-encode-decode.t)
      return 0 ;;
    *)
      return 1 ;;
  esac
}

test_file="$1"
file_timeout=$(per_file_timeout "$test_file")
if needs_roast_cwd "$test_file"; then
  abs_bin="$(cd "$(dirname "$MUTSU_BIN")" && pwd)/$(basename "$MUTSU_BIN")"
  abs_test="$(cd "$(dirname "$test_file")" && pwd)/$(basename "$test_file")"
  cd roast || exit 1
  exec timeout "$file_timeout" "$abs_bin" "$abs_test"
fi
exec timeout "$file_timeout" "$MUTSU_BIN" "$test_file"
