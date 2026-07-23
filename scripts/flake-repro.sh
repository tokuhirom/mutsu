#!/bin/bash
# Verify a suspected-flaky test by running it repeatedly, under CI-like load.
#
# Usage:
#   scripts/flake-repro.sh [-n RUNS] [-l LOAD] [-e ENV=VAL ...] <test-file> [more test files]
#
#   -n RUNS   how many times to run each file (default 20)
#   -l LOAD   how many CPU-burner processes to run alongside (default 0).
#             CI runs `prove -j4` on a 4-core runner, so `-l 4` approximates the
#             contention a timing-sensitive test actually sees there.
#   -e        extra environment for the run, repeatable, e.g.
#             `-e MUTSU_JIT=on -e MUTSU_JIT_THRESHOLD=2` for the jit-stress job.
#
# Prints a pass/fail tally per file and exits non-zero if ANY file failed at
# least once. A file that fails every single run is NOT flaky -- it is a
# deterministic bug; the tally makes that distinction visible instead of
# leaving it to a hunch.
#
# This is the evidence a flaky-tests.txt entry must cite. See
# docs/flaky-test-policy.md.

set -uo pipefail

RUNS=20
LOAD=0
declare -a EXTRA_ENV=()

while [ $# -gt 0 ]; do
  case "$1" in
    -n) RUNS="$2"; shift 2 ;;
    -l) LOAD="$2"; shift 2 ;;
    -e) EXTRA_ENV+=("$2"); shift 2 ;;
    -h|--help) sed -n '2,25p' "$0"; exit 0 ;;
    *) break ;;
  esac
done

if [ $# -eq 0 ]; then
  echo "usage: $0 [-n RUNS] [-l LOAD] [-e ENV=VAL] <test-file>..." >&2
  exit 2
fi

MUTSU_BIN="${MUTSU_BIN:-target/release/mutsu}"
if [ ! -x "$MUTSU_BIN" ]; then
  echo "error: $MUTSU_BIN not found. Run: cargo build --release" >&2
  exit 2
fi

declare -a LOAD_PIDS=()
cleanup() {
  for pid in ${LOAD_PIDS+"${LOAD_PIDS[@]}"}; do kill "$pid" 2>/dev/null; done
}
trap cleanup EXIT

if [ "$LOAD" -gt 0 ]; then
  echo "Starting $LOAD background CPU burners to emulate CI contention..." >&2
  for _ in $(seq "$LOAD"); do
    ( while :; do :; done ) &
    LOAD_PIDS+=($!)
  done
fi

overall=0
for test_file in "$@"; do
  pass=0; fail=0
  declare -a codes=()
  for i in $(seq "$RUNS"); do
    if [[ "$test_file" == roast/* ]]; then
      # Same wrapper CI uses: per-file timeout budget + MUTSU_FUDGE.
      # FLAKY_LIST=/dev/null disables the quarantine retry: this script exists to
      # MEASURE the raw failure rate, so it must see every failure, including
      # ones CI would silently re-roll.
      out=$(env "${EXTRA_ENV[@]+"${EXTRA_ENV[@]}"}" MUTSU_BIN="$MUTSU_BIN" \
        FLAKY_LIST=/dev/null scripts/run-roast-test.sh "$test_file" 2>&1)
    else
      out=$(env "${EXTRA_ENV[@]+"${EXTRA_ENV[@]}"}" \
        timeout 60 "$MUTSU_BIN" "$test_file" 2>&1)
    fi
    rc=$?
    # A `not ok ... # TODO` line is an *expected* failure (a fudged roast
    # subtest), not a real one, and does not make the process exit non-zero. So
    # count a run as failed only on a non-zero exit or a `not ok` WITHOUT a TODO
    # marker -- otherwise every TODO-carrying roast file looks permanently
    # broken. mutsu exits non-zero on genuine test failures ("Test failures").
    if [ $rc -eq 0 ] && ! grep -E '^not ok' <<< "$out" | grep -qvE '# *TODO'; then
      pass=$((pass + 1))
    else
      fail=$((fail + 1))
      codes+=("run#$i rc=$rc")
      # Keep the first failing output around for diagnosis.
      if [ "$fail" -eq 1 ]; then
        mkdir -p tmp/flake-repro
        printf '%s\n' "$out" > "tmp/flake-repro/$(echo "$test_file" | tr / _).log"
      fi
    fi
  done
  verdict="DETERMINISTIC PASS"
  [ "$fail" -gt 0 ] && verdict="FLAKY ($fail/$RUNS failed)"
  [ "$pass" -eq 0 ] && verdict="DETERMINISTIC FAIL"
  printf '%-56s pass=%-3d fail=%-3d %s\n' "$test_file" "$pass" "$fail" "$verdict"
  if [ "$fail" -gt 0 ]; then
    printf '    %s\n' "${codes[*]}"
    printf '    first failing output: tmp/flake-repro/%s.log\n' "$(echo "$test_file" | tr / _)"
    overall=1
  fi
  unset codes
done

exit $overall
