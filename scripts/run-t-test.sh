#!/bin/bash
# Wrapper for running a single t/ test with a per-file timeout and the flaky
# quarantine retry. The roast suite has had `scripts/run-roast-test.sh` for a
# while; this is the t/ counterpart, so both suites go through one retry engine.
#
# Used via: prove -r -e 'scripts/run-t-test.sh' t/   (-r: t/ is a nested tree,
# see docs/t-directory-layout.md)
#
# Usage: scripts/run-t-test.sh <test-file>
#
#   MUTSU_BIN        interpreter to run (default target/debug/mutsu)
#   MUTSU_T_TIMEOUT  per-file timeout in seconds (default 30)

MUTSU_BIN="${MUTSU_BIN:-target/debug/mutsu}"
MUTSU_T_TIMEOUT="${MUTSU_T_TIMEOUT:-30}"

# Integer multiplier on MUTSU_T_TIMEOUT for a file whose own work is
# deterministically heavy, the t/ counterpart of run-roast-test.sh's
# `per_file_timeout`. A multiplier rather than a fixed budget, because each
# caller sets its base for its binary (30 by default, 60 for the release
# `make test`, 90 for the debug CI jobs), and a heavy file is heavy under all
# of them. Only for a file whose cost is measured and understood; a slow file
# whose cost is a defect gets fixed, not a bigger budget
# (docs/flaky-test-policy.md 3).
per_file_timeout_scale() {
  case "$1" in
    t/collections/lazy-seq/lazy-generator-scan-strict-force.t)
      # Its first assertion strict-forces an endpoint-less closure sequence,
      # which drives the generator for the full 1_000_000-element bounded
      # attempt (CLOSURE_SEQ_EAGER_CAP, the same cap the map/grep pipe force
      # uses) before answering X::Cannot::Lazy. Measured: 1.6s on a release
      # binary, 25s on an opt-level-0 debug binary (the 1M generator calls are
      # 20.5s of it); every other X::Cannot::Lazy test in t/ runs under 1s.
      # Under `prove -j4` on a 4-core runner the stress jobs' 90s debug budget
      # timed it out with 0 tests reported (#11090).
      echo 3
      ;;
    *)
      echo 1
      ;;
  esac
}

test_file="$1"
file_timeout=$((MUTSU_T_TIMEOUT * $(per_file_timeout_scale "$test_file")))

# TEMP-DIAG (#12165): four NativeCall tests exit 1 with no output on CI only
# (a few ms, no TAP plan, identical on a re-run, not reproducible locally). When
# a file fails WITHOUT having printed a plan, leave a report in
# $MUTSU_CRASH_DIR -- `report-crash-reports.sh` prints those into the job log --
# with what the process saw and a second run of the same file with its stderr
# kept. Remove this block once the cause is found.
out=$(mktemp)
scripts/flaky-retry.sh "$test_file" \
  timeout "$file_timeout" "$MUTSU_BIN" "$test_file" 2>&1 | tee "$out"
rc=${PIPESTATUS[0]}
if [ "$rc" -ne 0 ] && ! grep -q '^1\.\.' "$out" && [ -n "${MUTSU_CRASH_DIR:-}" ]; then
  mkdir -p "$MUTSU_CRASH_DIR"
  {
    echo "argv: DIAG plan-less failure of $test_file"
    echo "rc=$rc pwd=$(pwd) bin=$MUTSU_BIN user=$(id -un) uid=$(id -u)"
    ls -la "$MUTSU_BIN" 2>&1
    "$MUTSU_BIN" --version 2>&1 | head -3
    echo "--- output of the failed run:"
    cat "$out"
    echo "--- the same file again, stderr kept (timeout ${file_timeout}s):"
    timeout "$file_timeout" "$MUTSU_BIN" "$test_file" 2>&1 | head -40
    echo "second run rc=${PIPESTATUS[0]}"
    echo "--- cache dirs:"
    ls -ld "$HOME" "$HOME/.cache" "$HOME/.cache/mutsu" "$HOME/.cache/mutsu/precomp" 2>&1
    ls "$HOME/.cache/mutsu/precomp" 2>&1 | wc -l
    echo "--- env:"
    env | grep -E '^(MUTSU|HOME|PATH|LANG|LC_|TMPDIR|RUST|XDG)' | sort
    echo "--- limits:"
    ulimit -a 2>&1
    echo "--- disk:"
    df -h . /tmp "$HOME" 2>&1
  } > "$MUTSU_CRASH_DIR/diag-$$.txt" 2>&1
fi
rm -f "$out"
exit "$rc"
