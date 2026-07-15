#!/bin/bash
# Single-pass wall-clock comparison of raku vs mutsu (release) across the
# whitelisted roast tests, to surface where mutsu is *slower* than raku.
#
# Rationale: mutsu starts in ~0.01s and raku recompiles its setting + Test
# module on every invocation (~0.3-0.6s), so on short tests mutsu wins purely on
# startup. The interesting signal is therefore `ratio > 1` -- a file where mutsu
# is slower *despite* that startup advantage is a genuine engine-execution gap.
#
# This is a COARSE, single-run measurement (the goal is finding obvious gaps,
# not precise timing). Run it serially -- do NOT launch two copies at once, or
# they contend for CPU and the absolute times become noise (the per-row ratio,
# measured back-to-back, stays usable). Re-confirm any outlier with a clean,
# isolated single run before acting on the absolute number.
#
# Usage: cargo build --release && scripts/roast-speed-diff.sh
# Output: tmp/roast-speed.tsv  (path, mutsu_s, raku_s, ratio, status)
# Analyse: sort -t$'\t' -k4 -rn tmp/roast-speed.tsv | awk -F'\t' '$4>1'
set -u
cd "$(dirname "$0")/.." || exit 1
BIN="${MUTSU_BIN:-target/release/mutsu}"
INC=roast/packages/Test-Helpers/lib
OUT="${OUT:-tmp/roast-speed.tsv}"
CAP="${CAP:-30}"          # uniform per-run timeout (sleep-bound tests hit this)
LIST="${1:-roast-whitelist.txt}"
mkdir -p "$(dirname "$OUT")"
echo -e "path\tmutsu_s\traku_s\tratio\tstatus" > "$OUT"
n=0
while IFS= read -r f; do
  [ -f "$f" ] || continue
  n=$((n+1))
  t0=$(date +%s.%N); timeout "$CAP" env MUTSU_FUDGE=1 "$BIN" "$f" >/dev/null 2>&1; mrc=$?; t1=$(date +%s.%N)
  r0=$(date +%s.%N); timeout "$CAP" raku -I "$INC" "$f" >/dev/null 2>&1; rrc=$?; r1=$(date +%s.%N)
  st="ok"
  [ $mrc -eq 124 ] && st="mutsu_timeout"
  [ $rrc -eq 124 ] && st="raku_timeout"
  awk -v f="$f" -v a=$t0 -v b=$t1 -v c=$r0 -v d=$r1 -v s="$st" \
    'BEGIN{m=b-a;r=d-c;printf "%s\t%.3f\t%.3f\t%s\t%s\n",f,m,r,(r>0.001?sprintf("%.2f",m/r):"NA"),s}' >> "$OUT"
  [ $((n % 100)) -eq 0 ] && echo "…$n done" >&2
done < "$LIST"
echo "DONE: $n files -> $OUT" >&2
