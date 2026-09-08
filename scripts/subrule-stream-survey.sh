#!/usr/bin/env bash
# Aggregate the `MUTSU_VM_STATS` subrule-stream verdict histogram across many
# scripts, so the six declined shapes in
# https://github.com/tokuhirom/mutsu/issues/7548 can be ranked by how often they
# actually occur rather than by how plausible they sound.
#
# Every `<subrule>` call that reaches `drive_named_subrule_candidates` reports
# exactly one verdict: `streamed`, or the shape that declined it. The streamed
# path is *correct* today, so what clearing a residue buys is only fewer
# `{ ... }` block runs on paths raku never enters -- which makes the per-call
# counts the deciding measurement.
#
# Usage:
#   scripts/subrule-stream-survey.sh <file.t|file.raku> ...
#   scripts/subrule-stream-survey.sh $(cat roast-whitelist.txt)
#
# Honours MUTSU_BIN (default target/release/mutsu) and MUTSU_JOBS (default 8).
# Roast files are run with MUTSU_FUDGE=1, as `make roast` does.
set -u

BIN="${MUTSU_BIN:-target/release/mutsu}"
JOBS="${MUTSU_JOBS:-8}"
TIMEOUT="${MUTSU_SURVEY_TIMEOUT:-60}"

if [ ! -x "$BIN" ]; then
    echo "no such binary: $BIN (set MUTSU_BIN, or cargo build --release)" >&2
    exit 2
fi
if [ "$#" -eq 0 ]; then
    echo "usage: $0 <script> ..." >&2
    exit 2
fi

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

run_one() {
    local file="$1" fudge=""
    case "$file" in
        roast/*) fudge=1 ;;
    esac
    MUTSU_VM_STATS=1 MUTSU_FUDGE="$fudge" timeout "$TIMEOUT" "$BIN" "$file" 2>&1 >/dev/null |
        grep -F '[mutsu vm-stats] subrule-stream verdicts'
}
export -f run_one
export BIN TIMEOUT

printf '%s\n' "$@" | xargs -P "$JOBS" -I{} bash -c 'run_one "$@"' _ {} > "$work/lines" 2>/dev/null

files_with_calls=$(wc -l < "$work/lines")
echo "files reporting at least one subrule call: $files_with_calls (of $#)"
echo

# A line reads `... total=N streamed=M (P%): reason=count reason=count ...`.
# Drop everything through the `): ` so the header's own `total=`/`streamed=` are
# not summed a second time alongside the per-reason body.
sed 's/^.*): //' "$work/lines" |
    tr ' ' '\n' |
    grep -E '^[a-z-]+=[0-9]+$' |
    awk -F= '{ n[$1] += $2; total += $2 }
             END {
                 for (r in n) printf "%-26s %12d  %6.2f%%\n", r, n[r], 100 * n[r] / total
                 printf "%-26s %12d\n", "TOTAL", total
             }' |
    sort -k2 -nr
