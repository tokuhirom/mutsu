#!/usr/bin/env bash
# Measure how Str positional methods scale: run each case at N and 2N calls on
# an N-/2N-character string and print t(2N)/t(N). A ratio near 2 is linear in
# the total work (O(1) per call), near 4 is quadratic (O(n) per call).
#
# Usage: scripts/str-complexity-check.sh [binary] [case-filter-regex]
#   binary defaults to target/release/mutsu; pass `raku` to measure rakudo.
# See https://github.com/tokuhirom/mutsu/issues/9140.
set -euo pipefail

BIN="${1:-target/release/mutsu}"
FILTER="${2:-.}"

# name | N | setup (uses $n) | loop body (uses $s and $i)
CASES=(
  'chars-wide|100000|my $s = "あ" x $n;|$s.chars'
  'substr-wide|100000|my $s = "あ" x $n;|$s.substr($i, 1)'
  'substr-ascii|200000|my $s = "a" x $n;|$s.substr($i, 1)'
  'index-pos|100000|my $s = "あ" x $n;|$s.index("あ", $i)'
  'rindex-pos|100000|my $s = "あ" x $n;|$s.rindex("あ", $i)'
  'contains-pos|100000|my $s = "あ" x $n;|$s.contains("あ", $i)'
  'substr-eq|200000|my $s = "あ" x $n;|$s.substr-eq("あ", $i)'
  'starts-with|500000|my $s = "あ" x $n;|$s.starts-with("あ")'
  'ord|500000|my $s = "あ" x $n;|$s.ord'
  'indices|100000|my $s = "a" x $n;|$s.indices("a") if $i == 0'
)

time_case() {
  local n="$1" setup="$2" body="$3"
  local code="my \$n = $n; $setup my \$t = now; for ^\$n -> \$i { $body }; say (now - \$t).round(0.001)"
  timeout 300 "$BIN" -e "$code"
}

printf '%-14s %7s %8s %8s %6s\n' case N 't(N)' 't(2N)' ratio
for c in "${CASES[@]}"; do
  IFS='|' read -r name n setup body <<<"$c"
  [[ "$name" =~ $FILTER ]] || continue
  t1=$(time_case "$n" "$setup" "$body")
  t2=$(time_case "$((n * 2))" "$setup" "$body")
  ratio=$(awk -v a="$t1" -v b="$t2" 'BEGIN { if (a > 0) printf "%.2f", b / a; else print "-" }')
  printf '%-14s %7d %8s %8s %6s\n' "$name" "$n" "$t1" "$t2" "$ratio"
done
