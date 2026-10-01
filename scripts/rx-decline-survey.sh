#!/usr/bin/env bash
# Sum the compiled regex engine's `MUTSU_VM_STATS` lines across many scripts:
# how many patterns compiled, how many declined to the tree walk, and why; and
# every run-time use of the walk's code (`regex-walk:`), by group and reason.
#
# The per-reason counts are ADR-0135 D5's migration ratchet
# (docs/adr/0135-regex-compiles-to-a-backtracking-program.md): each slice PR
# quotes this survey before and after its change, and the walk is deleted when
# `declined` reads zero.
#
# Each script reports one line at exit:
#   [mutsu vm-stats] regex-vm: compiled=N declined=M runs=R reasons=(r=n …)
# Counts are per process (a pattern compiled once per run), so the totals are
# "pattern compilations summed over files", not distinct patterns. A file that
# spawns mutsu children (`is_run`, `run $*EXECUTABLE`) reports their lines too.
#
# Usage:
#   scripts/rx-decline-survey.sh                  # all of t/ plus the roast whitelist
#   scripts/rx-decline-survey.sh <file.t> ...     # just these files
#
# Honours MUTSU_BIN (default target/release/mutsu), MUTSU_JOBS (default: the
# number of cores) and MUTSU_SURVEY_TIMEOUT (seconds per file, default 60).
# Roast files run with MUTSU_FUDGE=1, as `make roast` does.
set -u

BIN="${MUTSU_BIN:-target/release/mutsu}"
JOBS="${MUTSU_JOBS:-$(nproc 2>/dev/null || echo 4)}"
TIMEOUT="${MUTSU_SURVEY_TIMEOUT:-60}"

if [ ! -x "$BIN" ]; then
    echo "no such binary: $BIN (set MUTSU_BIN, or cargo build --release)" >&2
    exit 2
fi

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

if [ "$#" -eq 0 ]; then
    find t -name '*.t' | LC_ALL=C sort > "$work/files"
    grep -v '^[[:space:]]*\(#\|$\)' roast-whitelist.txt >> "$work/files"
else
    printf '%s\n' "$@" > "$work/files"
fi

run_one() {
    local file="$1" fudge=""
    case "$file" in
        roast/*) fudge=1 ;;
    esac
    MUTSU_VM_STATS=1 MUTSU_FUDGE="$fudge" timeout "$TIMEOUT" "$BIN" "$file" \
        2>&1 >/dev/null </dev/null |
        grep -oE 'regex-vm: compiled=[0-9]+ declined=[0-9]+ runs=[0-9]+ reasons=\([^)]*\)|regex-walk: .*'
}
export -f run_one
export BIN TIMEOUT

xargs -P "$JOBS" -I{} bash -c 'run_one "$@"' _ {} < "$work/files" > "$work/lines" 2>/dev/null

echo "files: $(wc -l < "$work/files"), stats lines: $(wc -l < "$work/lines")"

# Header totals: compiled / declined / runs.
grep '^regex-vm:' "$work/lines" > "$work/vm"
grep '^regex-walk:' "$work/lines" > "$work/walk"
sed -n 's/^regex-vm: compiled=\([0-9]*\) declined=\([0-9]*\) runs=\([0-9]*\).*/\1 \2 \3/p' \
    "$work/vm" |
    awk '{ c += $1; d += $2; r += $3 }
         END {
             t = c + d
             printf "compiled=%d declined=%d (%.1f%% compiled) runs=%d\n",
                    c, d, t ? 100 * c / t : 0, r
         }'
echo

# Per-reason body: everything inside `reasons=( … )`.
sed -n 's/^.*reasons=(\([^)]*\))$/\1/p' "$work/vm" |
    tr ' ' '\n' |
    grep -E '^[A-Za-z0-9_-]+=[0-9]+$' |
    awk -F= '{ n[$1] += $2; total += $2 }
             END {
                 for (r in n) printf "%-26s %8d  %6.2f%%\n", r, n[r], 100 * n[r] / total
             }' |
    sort -k2 -nr

# Every use of the walk's code (ADR-0135 §8, Slice E): the `regex-walk:` line,
# `walked=N (reason=n …) bridged=M (…) leaf=L (…)`, summed per group and reason.
echo
walk_fields() {
    awk '{
             for (i = 2; i <= NF; i++) {
                 f = $i
                 gsub(/[()]/, "", f)
                 if (f !~ /^[A-Za-z0-9_:-]+=[0-9]+$/) continue
                 split(f, kv, "=")
                 if ($i ~ /^(walked|bridged|leaf)=/) { group = kv[1]; print "total", group, kv[2] }
                 else print group, kv[1], kv[2]
             }
         }' "$work/walk"
}
walk_fields | awk '$1 == "total" { t[$2] += $3 }
                   END { printf "walk uses: walked=%d bridged=%d leaf=%d\n", t["walked"], t["bridged"], t["leaf"] }'
walk_fields | awk '$1 != "total" { n[$1 " " $2] += $3 }
                   END { for (k in n) { split(k, g, " "); printf "%-8s %-30s %10d\n", g[1], g[2], n[k] } }' |
    sort -k1,1 -k3nr
