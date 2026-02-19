#!/bin/bash
# Pick the next roast test to work on, prioritized by severity.
# Priority: panic > timeout > error > fail
# Within each category, picks the shortest file (by line count).
# Excludes tests already in the whitelist.
#
# Usage: ./scripts/pick-next-roast.sh
#        ./scripts/pick-next-roast.sh -n 5   # show top 5 candidates

set -euo pipefail

OUTDIR="tmp"
WHITELIST="roast-whitelist.txt"
COUNT=5

while getopts "n:" opt; do
    case "$opt" in
        n) COUNT="$OPTARG" ;;
        *) echo "Usage: $0 [-n count]" >&2; exit 1 ;;
    esac
done

CATEGORIES=(
    "panic:$OUTDIR/roast-panic.txt"
    "timeout:$OUTDIR/roast-timeout.txt"
    "error:$OUTDIR/roast-error.txt"
    "fail:$OUTDIR/roast-fail.txt"
)

found=0

for entry in "${CATEGORIES[@]}"; do
    label="${entry%%:*}"
    file="${entry#*:}"

    if [[ ! -f "$file" ]]; then
        continue
    fi

    candidates=$(comm -23 <(sort "$file") <(sort "$WHITELIST") \
        | while read -r f; do
            if [[ -f "$f" ]]; then
                echo "$(wc -l < "$f") $f"
            fi
        done \
        | sort -n)

    if [[ -z "$candidates" ]]; then
        continue
    fi

    echo "=== $label ==="
    echo "$candidates" | head -"$COUNT"
    found=$((found + $(echo "$candidates" | head -"$COUNT" | wc -l)))

    if [[ "$found" -ge "$COUNT" ]]; then
        break
    fi
done

if [[ "$found" -eq 0 ]]; then
    echo "No failing tests found. Run ./scripts/roast-history.sh first." >&2
    exit 1
fi
