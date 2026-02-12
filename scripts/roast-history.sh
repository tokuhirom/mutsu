#!/bin/bash
# Run all roast tests and append results to HISTORY.tsv
# Usage: ./scripts/roast-history.sh

set -uo pipefail

cd "$(dirname "$0")/.."

MUTSU=./target/release/mutsu
HISTORY=HISTORY.tsv
TIMEOUT=10  # seconds per test file

# Build first
echo "Building mutsu (release with debug symbols)..."
cargo build --release 2>&1 | tail -1

# Collect test files (exclude 6.c, 6.d, integration, 3rdparty, APPENDICES)
mapfile -t TEST_FILES < <(
  find roast -maxdepth 2 -name '*.t' \
    -not -path '*/6.c/*' \
    -not -path '*/6.d/*' \
    -not -path '*/integration/*' \
    -not -path '*/3rdparty/*' \
    -not -path '*/APPENDICES/*' \
    -not -path '*/MISC/*' \
    -not -path '*/packages/*' \
    -not -path '*/docs/*' \
  | sort
)

TOTAL=${#TEST_FILES[@]}
PASS=0
FAIL=0
ERROR=0
TIMEDOUT=0
TOTAL_SUBTESTS=0
PASSED_SUBTESTS=0

echo "Running $TOTAL roast test files (timeout=${TIMEOUT}s each)..."

for f in "${TEST_FILES[@]}"; do
  # Run with timeout, capture TAP output (tr -d to strip null bytes)
  EXIT_CODE=0
  OUTPUT=$(timeout "$TIMEOUT" "$MUTSU" "$f" 2>/dev/null | tr -d '\0') || EXIT_CODE=$?

  if [ "$EXIT_CODE" -eq 124 ]; then
    # timeout
    TIMEDOUT=$((TIMEDOUT + 1))
    continue
  fi

  # Parse TAP output
  PLAN=$(echo "$OUTPUT" | grep -oE '^1\.\.[0-9]+' | head -1 | sed 's/1\.\.//')
  # Sanitize: extract only digits
  PLAN=$(echo "$PLAN" | grep -oE '^[0-9]+$' || true)
  if [ -z "$PLAN" ]; then
    ERROR=$((ERROR + 1))
    continue
  fi

  OK_COUNT=$(echo "$OUTPUT" | grep -cE '^ok [0-9]' || true)
  NOT_OK_COUNT=$(echo "$OUTPUT" | grep -cE '^not ok [0-9]' || true)

  TOTAL_SUBTESTS=$((TOTAL_SUBTESTS + PLAN))
  PASSED_SUBTESTS=$((PASSED_SUBTESTS + OK_COUNT))

  if [ "$NOT_OK_COUNT" -eq 0 ] && [ "$EXIT_CODE" -eq 0 ] && [ "$OK_COUNT" -eq "$PLAN" ]; then
    PASS=$((PASS + 1))
  else
    FAIL=$((FAIL + 1))
  fi
done

DATE=$(date +%Y-%m-%d)
COMMIT=$(git rev-parse --short HEAD)

echo ""
echo "=== Results ==="
echo "Date:       $DATE"
echo "Commit:     $COMMIT"
echo "Files:      $TOTAL"
echo "Pass:       $PASS"
echo "Fail:       $FAIL"
echo "Error:      $ERROR"
echo "Timeout:    $TIMEDOUT"
echo "Subtests:   $PASSED_SUBTESTS / $TOTAL_SUBTESTS"

# Create header if file doesn't exist
if [ ! -f "$HISTORY" ]; then
  printf "date\tcommit\tfiles\tpass\tfail\terror\ttimeout\tsubtests_pass\tsubtests_total\n" > "$HISTORY"
fi

printf "%s\t%s\t%d\t%d\t%d\t%d\t%d\t%d\t%d\n" \
  "$DATE" "$COMMIT" "$TOTAL" "$PASS" "$FAIL" "$ERROR" "$TIMEDOUT" \
  "$PASSED_SUBTESTS" "$TOTAL_SUBTESTS" >> "$HISTORY"

echo ""
echo "Appended to $HISTORY"
