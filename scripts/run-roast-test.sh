#!/bin/bash
# Wrapper for running a single roast test with a per-file timeout.
# Used by `make roast` via: prove -e 'MUTSU_BIN=... scripts/run-roast-test.sh'
#
# Usage: scripts/run-roast-test.sh <test-file>

MUTSU_BIN="${MUTSU_BIN:-target/release/mutsu}"
DEFAULT_TIMEOUT=30

# Roast tests rely on fudge directives (#?rakudo skip/todo, #?DOES, #?v6, ...).
# mutsu only preprocesses them when MUTSU_FUDGE is set, so that a stray
# `#?rakudo skip` in ordinary user code does not silently drop statements.
export MUTSU_FUDGE=1

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
    roast/S17-supply/syntax.t)
      # 90 concurrency-heavy subtests: a ^500 react/channel stress loop
      # (1000 short-lived threads), Supply.interval polling, and fixed
      # Promise.in waits. Wall-clock is highly load-sensitive (measured
      # 19-35s run-to-run on an idle 12-core box, debug and release alike,
      # identical on main); under `prove -j4` on a 4-core CI runner the
      # default 30s budget times it out mid-file (exit 124, Failed: 0)
      # while the gc-stress job's 2x-scaled 60s budget passes. Same
      # rationale as S17-promise/start.t above.
      echo 60
      ;;
    roast/S17-promise/allof.t)
      # This test uses sleep 2*$_ with $_ up to 9, parallel start blocks take ~18s.
      echo 60
      ;;
    roast/S17-promise/start.t)
      # Spawns ^300 `start` blocks plus several `sleep 1` waits. Standalone it
      # runs in ~7.5s even under the gc-stress config (GC=on + VERIFY, release),
      # but under `prove -j4` the 300-thread spawn contends for cores with the
      # other three roast processes and blew the default 30s (x2 = 60s under
      # MUTSU_ROAST_TIMEOUT_SCALE) budget at 49/65 tests. Give it explicit
      # headroom so parallel CPU contention can't time it out. See
      # docs/gc-level1-detailed-design.md 9.3a.
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
    roast/integration/advent2012-day21.t)
      # A Collatz benchmark shoot-out: 8 sub-programs each computing 402
      # collatz lengths in a subprocess (Test::Util::run). Pure wall-clock
      # stress (~100s debug); give release headroom under -j4 contention.
      echo 120
      ;;
    roast/integration/advent2013-day14.t)
      # Promise/Channel pipeline with fixed sleeps (sleep 1/3/7 exchanges
      # capped by Promise.in(5), plus a 2s .then chain) — ~12s of wall-clock
      # waiting regardless of build, needs headroom under CI load.
      echo 60
      ;;
    roast/APPENDICES/A01-limits/misc.t)
      # Declares a 4 GiB native string ("a" x 2**32-1): allocation +
      # doubling-memcpy build takes ~9s locally and is memory-bandwidth
      # bound, so give CI runners headroom.
      echo 90
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
# MUTSU_ROAST_TIMEOUT_SCALE: integer multiplier on every per-file timeout.
# The CI gc-stress job sets 2: with MUTSU_GC=on + MUTSU_GC_VERIFY=1 the suite
# runs ~2x slower across the board (collector scans + verify snapshots), so
# fixed budgets tuned for GC-off leave no headroom — S17-lowlevel/thread.t
# measured 23.5s locally against the 30s default and timed out on a loaded
# runner the first time the GC=on roast step ran as a blocking gate. Scaling
# the whole schedule encodes "same tests, slower mode" once, instead of
# per-file whack-a-mole after each flake.
if [[ "${MUTSU_ROAST_TIMEOUT_SCALE:-1}" =~ ^[0-9]+$ ]] && [ "${MUTSU_ROAST_TIMEOUT_SCALE:-1}" -gt 1 ]; then
  file_timeout=$((file_timeout * MUTSU_ROAST_TIMEOUT_SCALE))
fi
if needs_roast_cwd "$test_file"; then
  abs_bin="$(cd "$(dirname "$MUTSU_BIN")" && pwd)/$(basename "$MUTSU_BIN")"
  abs_test="$(cd "$(dirname "$test_file")" && pwd)/$(basename "$test_file")"
  cd roast || exit 1
  exec timeout "$file_timeout" "$abs_bin" "$abs_test"
fi
exec timeout "$file_timeout" "$MUTSU_BIN" "$test_file"
