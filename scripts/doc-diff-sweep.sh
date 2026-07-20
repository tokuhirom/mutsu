#!/usr/bin/env bash
#
# doc-diff-sweep.sh — run the doc-diff harness over the WHOLE raku-doc corpus in
# parallel, one report per file, plus an aggregate summary sorted by signal.
#
# The harness (scripts/doc-diff-harness.raku) compares each runnable raku-doc
# example under reference `raku` (the oracle) vs `mutsu` and reports divergences.
# A single harness invocation processes its files serially, so sweeping the full
# ~440-file corpus that way takes hours. This driver fans the corpus out across
# `-P` worker processes; the harness writes to a per-PID scratch file
# (tmp/ddh/prog-<pid>.raku) so concurrent workers never race.
#
# Usage:
#   scripts/doc-diff-sweep.sh [-j N] [-o OUTDIR] [-m MUTSU] [ROOT ...]
#     -j N       parallel workers          (default: 8)
#     -o OUTDIR  report directory          (default: tmp/sweep)
#     -m MUTSU   mutsu binary              (default: target/debug/mutsu)
#     ROOT ...   files or dirs to scan     (default: raku-doc/doc/Type
#                                                    raku-doc/doc/Language)
#
# Outputs:
#   OUTDIR/reports/<sanitized-path>.txt   one harness report per file
#   OUTDIR/progress.txt                   "<file> :: # stats: ..." per file
#   OUTDIR/summary.txt                    corpus totals + files ranked by
#                                         (mismatch + crash), high-signal first
#
# Re-verify each finding directly before treating it as a real bug — the oracle
# gate keeps the report honest, but doc examples drift and some divergences are
# raku-version drift (bucketed separately as `raku-drift-from-doc`).
set -u

JOBS=8
OUTDIR="tmp/sweep"
MUTSU="target/debug/mutsu"
while getopts "j:o:m:" opt; do
  case "$opt" in
    j) JOBS="$OPTARG" ;;
    o) OUTDIR="$OPTARG" ;;
    m) MUTSU="$OPTARG" ;;
    *) echo "usage: $0 [-j N] [-o OUTDIR] [-m MUTSU] [ROOT ...]" >&2; exit 2 ;;
  esac
done
shift $((OPTIND - 1))

ROOTS=("$@")
if [ "${#ROOTS[@]}" -eq 0 ]; then
  ROOTS=(raku-doc/doc/Type raku-doc/doc/Language)
fi

REPORTS="$OUTDIR/reports"
mkdir -p "$REPORTS"
FILELIST="$OUTDIR/all-files.txt"
find "${ROOTS[@]}" -name '*.rakudoc' | sort > "$FILELIST"
echo "Sweeping $(wc -l < "$FILELIST") files with -j$JOBS ..." >&2

run_one() {
  local f="$1"
  local safe out stats
  safe=$(echo "$f" | sed 's|raku-doc/doc/||; s|/|__|g; s|\.rakudoc$||')
  out="$REPORTS/${safe}.txt"
  timeout 300 raku scripts/doc-diff-harness.raku --mutsu="$MUTSU" --report="$out" "$f" >/dev/null 2>&1
  stats=$(grep -m1 'stats:' "$out" 2>/dev/null || echo "# stats: (no report)")
  echo "${f} :: ${stats}"
}
export -f run_one
export REPORTS MUTSU

xargs -P "$JOBS" -I{} bash -c 'run_one "$@"' _ {} < "$FILELIST" \
  > "$OUTDIR/progress.txt" 2>&1

# Aggregate: corpus totals + files ranked by (mismatch + crash), high-signal first.
# Portable token parse (no gawk 3-arg match); the stats line is a run of key=value
# tokens, so split and read each key.
awk '
  / :: / {
    idx = index($0, " :: "); file = substr($0, 1, idx - 1)
    m = 0; mm = 0; cr = 0; dr = 0
    nn = split(substr($0, idx), T, " ")
    for (k = 1; k <= nn; k++) {
      if      (T[k] ~ /^match=/)        { s = T[k]; sub(/^match=/, "", s);        m  = s + 0 }
      else if (T[k] ~ /^mismatch=/)     { s = T[k]; sub(/^mismatch=/, "", s);     mm = s + 0 }
      else if (T[k] ~ /^mutsu-crash=/)  { s = T[k]; sub(/^mutsu-crash=/, "", s);  cr = s + 0 }
      else if (T[k] ~ /^raku-drift=/)   { s = T[k]; sub(/^raku-drift=/, "", s);   dr = s + 0 }
    }
    tm += m; tmm += mm; tcr += cr; tdr += dr
    sig = mm + cr
    if (sig > 0) printf "%4d  mism=%-3d crash=%-3d drift=%-3d  %s\n", sig, mm, cr, dr, file
  }
  END { printf "\n==== corpus totals ====\nmatch=%d  mismatch=%d  crash=%d  raku-drift=%d\n", tm, tmm, tcr, tdr }
' "$OUTDIR/progress.txt" | sort -rn > "$OUTDIR/summary.txt"

echo "SWEEP-DONE" >> "$OUTDIR/progress.txt"
echo "Reports: $REPORTS/    Summary: $OUTDIR/summary.txt" >&2
