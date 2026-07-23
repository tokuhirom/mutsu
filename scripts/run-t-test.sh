#!/bin/bash
# Wrapper for running a single t/ test with a per-file timeout and the flaky
# quarantine retry. The roast suite has had `scripts/run-roast-test.sh` for a
# while; this is the t/ counterpart, so both suites go through one retry engine.
#
# Used via: prove -e 'scripts/run-t-test.sh' t/
#
# Usage: scripts/run-t-test.sh <test-file>
#
#   MUTSU_BIN        interpreter to run (default target/debug/mutsu)
#   MUTSU_T_TIMEOUT  per-file timeout in seconds (default 30)

MUTSU_BIN="${MUTSU_BIN:-target/debug/mutsu}"
MUTSU_T_TIMEOUT="${MUTSU_T_TIMEOUT:-30}"

test_file="$1"

exec scripts/flaky-retry.sh "$test_file" \
  timeout "$MUTSU_T_TIMEOUT" "$MUTSU_BIN" "$test_file"
