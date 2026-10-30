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
    t/concurrency/concurrent-attr-hash-writes-do-not-corrupt.t|\
    t/concurrency/concurrent-param-hash-reader-families.t)
      # Deliberate hammer tests: 3-8 threads x 300-6000 iterations over one
      # shared container. Measured on an opt-level-0 debug binary, alone on
      # an idle 4-core box: 49-63s for attr-hash-writes (JIT/GC stress) and
      # 27-29s for param-hash-reader-families; 3-6s on a release binary. Under
      # the nightly stress jobs' `prove -j4` with a 90s base attr-hash-writes
      # ran out of budget (exit 124, "planned 8 ran 5") while every
      # reported assertion was ok and no crash report was written (#13344).
      # The hash twin (param-hash-reader-families) measured 27-29s alone on
      # a debug binary with the JIT forced hot and hit the same wall in the
      # nightly (exit 124, "planned 32 ran 29", no crash report; #13515).
      # The work is the point of the test, so the budget scales (x3).
      echo 3
      ;;
    t/rakuast/rakuast-deparse.t)
      # Nineteen deparses through rakudo's RakuAST::Deparse, whose 267
      # `multi method deparse` candidates are ranked against each node's
      # type on every dispatch. Measured on an opt-level-0 debug binary,
      # alone on an idle box: 106s (GC-stress and JIT-stress alike) after the
      # RakuAST conformance memo, 145s before it; ~4.4s on a release binary.
      # The debug binary also re-checks every memoised answer against the
      # general walk. The nightly stress jobs' 90s debug budget ran out at
      # case 10 (exit 124, "planned 19 ran 10", no crash report; #15598).
      # What remains is the per-dispatch pairwise tiering (todo:perf), so the
      # budget scales (x3) until that is gone.
      echo 3
      ;;
    *)
      echo 1
      ;;
  esac
}

test_file="$1"
file_timeout=$((MUTSU_T_TIMEOUT * $(per_file_timeout_scale "$test_file")))

exec scripts/flaky-retry.sh "$test_file" \
  timeout "$file_timeout" "$MUTSU_BIN" "$test_file"
