#!/bin/bash
# Extract roast test files that are expected to pass on Rakudo from spectest.data.
# Tests NOT listed in spectest.data are unlikely to pass on raku either,
# so mutsu should deprioritize them.
#
# Output: tmp/roast-raku-pass.txt (list of test files with roast/ prefix)
#
# Usage: ./scripts/roast-raku-check.sh

set -euo pipefail

cd "$(dirname "$0")/.."

OUTDIR=tmp
SPECTEST=roast/spectest.data

mkdir -p "$OUTDIR"

if [[ ! -f "$SPECTEST" ]]; then
  echo "Error: $SPECTEST not found" >&2
  exit 1
fi

# Extract test paths from spectest.data:
# - Skip comments and blank lines
# - Strip trailing comments (markers like # stress, # moar, # Perl)
# - Skip 6.c/, 6.d/, integration/, APPENDICES/, MISC/ (not in mutsu's scope)
# - Skip entries with # stress or # moar markers (platform/mode-specific)
# - Skip entries with # Perl marker (Perl 5 integration tests)
# - Prepend roast/ prefix
# - Only include files that actually exist
grep -v '^#' "$SPECTEST" \
  | grep -v '^$' \
  | grep -v '# stress' \
  | grep -v '# moar' \
  | grep -v '# Perl' \
  | grep -v '^6\.' \
  | grep -v '^integration/' \
  | grep -v '^APPENDICES/' \
  | grep -v '^MISC/' \
  | sed 's/[[:space:]]*#.*//' \
  | sed 's/[[:space:]]*$//' \
  | sed 's|^|roast/|' \
  | while read -r f; do
      if [[ -f "$f" ]]; then
        echo "$f"
      fi
    done \
  | sort > "$OUTDIR/roast-raku-pass.txt"

COUNT=$(wc -l < "$OUTDIR/roast-raku-pass.txt")
echo "Wrote $COUNT test paths to $OUTDIR/roast-raku-pass.txt"
