#!/bin/bash
# Run one t/ file as `EVAL(slurp($file).AST)` (scripts/ast-eval-driver.raku): the
# write direction of the RakuAST layer, with the per-file timeout and the flaky
# quarantine retry of scripts/run-t-test.sh.
#
# Usage: scripts/run-t-ast-eval.sh <test-file>
#
#   MUTSU_BIN        interpreter to run (default target/debug/mutsu)
#   MUTSU_T_TIMEOUT  per-file timeout in seconds (default 30)

MUTSU_BIN="${MUTSU_BIN:-target/debug/mutsu}"
MUTSU_T_TIMEOUT="${MUTSU_T_TIMEOUT:-30}"

test_file="$1"
exec scripts/flaky-retry.sh "$test_file" \
  timeout "$MUTSU_T_TIMEOUT" "$MUTSU_BIN" scripts/ast-eval-driver.raku "$test_file"
