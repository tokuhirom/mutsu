#!/usr/bin/env bash
# Run all roast tests against mutsu and report pass/fail counts.
# Usage: tools/run_all_roast.sh [--save]
#   --save  Append result summary to tools/roast_results.log
set -eu

ROAST_DIR="${ROAST_DIR:-../roast}"
MUTSU="./target/debug/mutsu"
TIMEOUT=10  # seconds per test

cargo build 2>/dev/null

total=0
pass=0
fail=0
error=0
skip=0

pass_list=""
fail_list=""

while IFS= read -r test_file; do
    rel="${test_file#$ROAST_DIR/}"
    total=$((total + 1))

    # Run with timeout, capture exit code
    output=$(timeout "$TIMEOUT" "$MUTSU" "$test_file" 2>/dev/null) && rc=0 || rc=$?

    if [ "$rc" -eq 124 ]; then
        # timeout
        fail=$((fail + 1))
        fail_list="$fail_list  TIMEOUT $rel\n"
        continue
    fi

    if [ "$rc" -ne 0 ]; then
        error=$((error + 1))
        continue
    fi

    # Check TAP output: look for "not ok" or missing plan
    if echo "$output" | grep -q "^not ok"; then
        fail=$((fail + 1))
        fail_list="$fail_list  NOTOK   $rel\n"
    elif echo "$output" | grep -q "^1\.\." || echo "$output" | grep -q "^ok "; then
        # Has TAP output and no "not ok" -> pass
        pass=$((pass + 1))
        pass_list="$pass_list  $rel\n"
    else
        # No recognizable TAP output but exited 0
        error=$((error + 1))
    fi
done < <(find "$ROAST_DIR" -name '*.t' -not -path '*/6.c/*' -not -path '*/6.d/*' -not -path '*/packages/*' -not -path '*/3rdparty/*' | sort)

echo "=== Roast Test Results ==="
echo "Total: $total"
echo "Pass:  $pass"
echo "Fail:  $fail (not ok or timeout)"
echo "Error: $error (runtime/parse error)"
echo "---"
echo "Pass rate: $pass / $total"
echo ""
echo "=== Passing tests ==="
printf "$pass_list"

if [ "${1:-}" = "--save" ]; then
    echo "$(date '+%Y-%m-%d %H:%M:%S') | pass=$pass fail=$fail error=$error total=$total" >> tools/roast_results.log
    echo "(saved to tools/roast_results.log)"
fi
