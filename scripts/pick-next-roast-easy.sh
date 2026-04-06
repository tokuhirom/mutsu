#!/bin/bash
# Pick the next EASY roast test to work on.
# Only looks at "fail" category (tests that run but have some failures).
# Sorts by pass rate (highest first) so codex gets near-passing tests.
# Excludes tests in whitelist, wip.txt, and too_difficult.txt.
#
# Usage: ./scripts/pick-next-roast-easy.sh
#        ./scripts/pick-next-roast-easy.sh -n 5

set -euo pipefail

OUTDIR="tmp"
WHITELIST="roast-whitelist.txt"
TOO_DIFFICULT="too_difficult.txt"
WIP_FILE="wip.txt"
COUNT=1
BINARY="target/debug/mutsu"

while getopts "n:" opt; do
    case "$opt" in
        n) COUNT="$OPTARG" ;;
        *) echo "Usage: $0 [-n count]" >&2; exit 1 ;;
    esac
done

FAIL_FILE="$OUTDIR/roast-fail.txt"
RAKU_PASS="$OUTDIR/roast-raku-pass.txt"

if [[ ! -f "$FAIL_FILE" ]]; then
    echo "No fail file found. Run ./scripts/roast-history.sh first." >&2
    exit 1
fi

# Filter out whitelist, wip, too_difficult
filtered=$(comm -23 <(sort "$FAIL_FILE") <(sort "$WHITELIST"))
if [[ -f "$TOO_DIFFICULT" ]]; then
    filtered=$(comm -23 <(echo "$filtered" | sort) <(sort "$TOO_DIFFICULT"))
fi
if [[ -f "$WIP_FILE" ]]; then
    filtered=$(comm -23 <(echo "$filtered" | sort) <(sort "$WIP_FILE"))
fi
if [[ -f "$RAKU_PASS" ]]; then
    filtered=$(comm -12 <(echo "$filtered" | sort) <(sort "$RAKU_PASS"))
fi

if [[ -z "$filtered" ]]; then
    echo "No easy candidates found." >&2
    exit 1
fi

# Pick short files (< 200 lines) — easier for codex
candidates=$(echo "$filtered" \
    | while read -r f; do
        if [[ -n "$f" ]] && [[ -f "$f" ]]; then
            lines=$(wc -l < "$f")
            if [[ "$lines" -lt 200 ]]; then
                echo "$lines $f"
            fi
        fi
    done \
    | sort -n -k1,1)

if [[ -z "$candidates" ]]; then
    # Fallback: allow longer files too
    candidates=$(echo "$filtered" \
        | while read -r f; do
            if [[ -n "$f" ]] && [[ -f "$f" ]]; then
                echo "$(wc -l < "$f") $f"
            fi
        done \
        | sort -n -k1,1)
fi

# Randomly pick from top 20 to avoid always hitting the same test
echo "$candidates" | head -20 | shuf | head -"$COUNT" | sed 's/^[0-9]* //'
