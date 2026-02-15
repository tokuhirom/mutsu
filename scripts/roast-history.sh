#!/bin/bash
# Run all roast tests and append results to HISTORY.tsv
# Also writes per-file categorization to tmp/roast-*.txt
# Usage: ./scripts/roast-history.sh

set -uo pipefail

cd "$(dirname "$0")/.."

MUTSU=./target/release/mutsu
HISTORY=HISTORY.tsv
TIMEOUT=10  # seconds per test file
OUTDIR=tmp

mkdir -p "$OUTDIR"

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
PANICKED=0
TOTAL_SUBTESTS=0
PASSED_SUBTESTS=0

# Clear category files
> "$OUTDIR/roast-pass.txt"
> "$OUTDIR/roast-fail.txt"
> "$OUTDIR/roast-error.txt"
> "$OUTDIR/roast-timeout.txt"
> "$OUTDIR/roast-panic.txt"

echo "Running $TOTAL roast test files (timeout=${TIMEOUT}s each)..."

for f in "${TEST_FILES[@]}"; do
  # Run with timeout, capture stdout and stderr separately
  EXIT_CODE=0
  STDERR_FILE=$(mktemp)
  OUTPUT=$(timeout "$TIMEOUT" "$MUTSU" "$f" 2>"$STDERR_FILE" | tr -d '\0') || EXIT_CODE=$?
  STDERR_CONTENT=$(<"$STDERR_FILE")
  rm -f "$STDERR_FILE"

  if [ "$EXIT_CODE" -eq 124 ]; then
    # timeout
    TIMEDOUT=$((TIMEDOUT + 1))
    echo "$f" >> "$OUTDIR/roast-timeout.txt"
    continue
  fi

  # Check for panic (Rust panic exit code is 101, or "panicked at" in stderr)
  if [ "$EXIT_CODE" -eq 101 ] || echo "$STDERR_CONTENT" | grep -q 'panicked at'; then
    PANICKED=$((PANICKED + 1))
    echo "$f" >> "$OUTDIR/roast-panic.txt"
    continue
  fi

  # Parse TAP output
  PLAN=$(echo "$OUTPUT" | grep -oE '^1\.\.[0-9]+' | head -1 | sed 's/1\.\.//')
  # Sanitize: extract only digits
  PLAN=$(echo "$PLAN" | grep -oE '^[0-9]+$' || true)
  if [ -z "$PLAN" ]; then
    ERROR=$((ERROR + 1))
    echo "$f" >> "$OUTDIR/roast-error.txt"
    continue
  fi

  OK_COUNT=$(echo "$OUTPUT" | grep -cE '^ok [0-9]' || true)
  NOT_OK_COUNT=$(echo "$OUTPUT" | grep -cE '^not ok [0-9]' || true)

  TOTAL_SUBTESTS=$((TOTAL_SUBTESTS + PLAN))
  PASSED_SUBTESTS=$((PASSED_SUBTESTS + OK_COUNT))

  if [ "$NOT_OK_COUNT" -eq 0 ] && [ "$EXIT_CODE" -eq 0 ] && [ "$OK_COUNT" -eq "$PLAN" ]; then
    PASS=$((PASS + 1))
    echo "$f" >> "$OUTDIR/roast-pass.txt"
  else
    FAIL=$((FAIL + 1))
    echo "$f" >> "$OUTDIR/roast-fail.txt"
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
echo "Panic:      $PANICKED"
echo "Timeout:    $TIMEDOUT"
echo "Subtests:   $PASSED_SUBTESTS / $TOTAL_SUBTESTS"

echo ""
echo "=== Category files ==="
echo "  $OUTDIR/roast-pass.txt     ($(wc -l < "$OUTDIR/roast-pass.txt") files)"
echo "  $OUTDIR/roast-fail.txt     ($(wc -l < "$OUTDIR/roast-fail.txt") files)"
echo "  $OUTDIR/roast-error.txt    ($(wc -l < "$OUTDIR/roast-error.txt") files)"
echo "  $OUTDIR/roast-panic.txt    ($(wc -l < "$OUTDIR/roast-panic.txt") files)"
echo "  $OUTDIR/roast-timeout.txt  ($(wc -l < "$OUTDIR/roast-timeout.txt") files)"

# Create header if file doesn't exist
if [ ! -f "$HISTORY" ]; then
  printf "date\tcommit\tfiles\tpass\tfail\terror\tpanic\ttimeout\tsubtests_pass\tsubtests_total\n" > "$HISTORY"
fi

printf "%s\t%s\t%d\t%d\t%d\t%d\t%d\t%d\t%d\t%d\n" \
  "$DATE" "$COMMIT" "$TOTAL" "$PASS" "$FAIL" "$ERROR" "$PANICKED" "$TIMEDOUT" \
  "$PASSED_SUBTESTS" "$TOTAL_SUBTESTS" >> "$HISTORY"

echo ""
echo "Appended to $HISTORY"

# Auto-commit only when the script reaches this point (completed run).
if git diff --quiet -- "$HISTORY"; then
  echo "No changes to commit in $HISTORY"
else
  git add "$HISTORY"
  git commit -m "Update roast history ($DATE)" -- "$HISTORY"
  echo "Committed $HISTORY"
fi
