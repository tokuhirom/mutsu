#!/bin/bash
# Scan non-whitelisted roast tests and record per-file (plan, failed) counts so
# near-pass files (1-2 failing subtests) can be prioritized for whitelisting.
# Output: tmp/near-pass.tsv  columns: failed<TAB>plan<TAB>ran<TAB>file
# Sorted by failed ascending (most-nearly-passing first).
set -uo pipefail
cd "$(dirname "$0")/.."

MUTSU=./target/release/mutsu
export MUTSU_FUDGE=1
TIMEOUT=15
OUT=tmp/near-pass.tsv
> "$OUT"

# All roast .t files NOT already whitelisted.
mapfile -t ALL < <(find roast -name '*.t' -type f | sort)
mapfile -t WL < <(sort roast-whitelist.txt)

declare -A IS_WL
for w in "${WL[@]}"; do IS_WL["$w"]=1; done

count=0
for f in "${ALL[@]}"; do
  [ -n "${IS_WL[$f]:-}" ] && continue
  count=$((count+1))
  EXIT_CODE=0
  OUTPUT=$(timeout "$TIMEOUT" "$MUTSU" "$f" 2>/dev/null | tr -d '\0') || EXIT_CODE=$?
  [ "$EXIT_CODE" -eq 124 ] && continue   # timeout: skip
  PLAN=$(echo "$OUTPUT" | grep -oE '^1\.\.[0-9]+' | head -1 | sed 's/1\.\.//')
  PLAN=$(echo "$PLAN" | grep -oE '^[0-9]+$' || true)
  [ -z "$PLAN" ] && continue             # no plan: skip (parse/error)
  OK=$(echo "$OUTPUT" | grep -cE '^ok [0-9]' || true)
  NOK=$(echo "$OUTPUT" | grep -E '^not ok [0-9]' | grep -cvE '# TODO' || true)
  RAN=$((OK + NOK))
  # Treat plan-not-reached as additional missing tests.
  MISSING=$((PLAN - RAN))
  [ "$MISSING" -lt 0 ] && MISSING=0
  TOTAL_BAD=$((NOK + MISSING))
  [ "$TOTAL_BAD" -eq 0 ] && continue      # fully passing (would already be whitelistable)
  printf '%d\t%d\t%d\t%s\n' "$TOTAL_BAD" "$PLAN" "$RAN" "$f" >> "$OUT"
done

sort -n -k1,1 "$OUT" -o "$OUT"
echo "Scanned $count non-whitelisted files. Near-pass candidates (failed<=3):"
awk -F'\t' '$1<=3' "$OUT"
