#!/usr/bin/env bash
# Count the distinct `Class.method` shapes that reach the slow-path resolver
# dispatch (`run_instance_method_celled`) over many scripts (#11510).
#
# The static ratchet (scripts/check-slow-path.py) bounds how much slow-path
# code can be written; this is the dynamic measure of how much of it still
# runs. Counting distinct shapes, not calls, keeps the number stable against
# test churn: it moves only when a mechanism is retired.
#
# Each script reports one line at exit under MUTSU_VM_STATS=1:
#   [mutsu vm-stats] slow-path-shapes: N Class.method=count Class.method=count ...
# A file that spawns mutsu children (`is_run`, `run $*EXECUTABLE`) reports
# their lines too. The union over all files is printed.
#
# Usage:
#   scripts/slow-path-survey.sh                  # all of t/ plus the roast whitelist
#   scripts/slow-path-survey.sh <file.t> ...     # just these files
#   scripts/slow-path-survey.sh --list           # also print every shape, with its calls
#
# Honours MUTSU_BIN (default target/release/mutsu), MUTSU_JOBS (default: the
# number of cores) and MUTSU_SURVEY_TIMEOUT (seconds per file, default 60).
# Roast files run with MUTSU_FUDGE=1, as `make roast` does.
set -u

BIN="${MUTSU_BIN:-target/release/mutsu}"
JOBS="${MUTSU_JOBS:-$(nproc 2>/dev/null || echo 4)}"
TIMEOUT="${MUTSU_SURVEY_TIMEOUT:-60}"
LIST=0
if [ "${1:-}" = "--list" ]; then
    LIST=1
    shift
fi

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
        grep -oE 'slow-path-shapes: [0-9]+ .*' |
        sed -E 's/^slow-path-shapes: [0-9]+ //'
}
export -f run_one
export BIN TIMEOUT

xargs -P "$JOBS" -I{} bash -c 'run_one "$@"' _ {} < "$work/files" > "$work/lines" 2>/dev/null

# One `Class.method=count` token per line, summed per shape.
tr ' ' '\n' < "$work/lines" | grep -E '^[^=]+=[0-9]+$' |
    awk -F= '{ calls[$1] += $2 } END { for (k in calls) print k, calls[k] }' |
    LC_ALL=C sort > "$work/shapes"

echo "files: $(wc -l < "$work/files"), distinct slow-path shapes: $(wc -l < "$work/shapes"), calls: $(awk '{ n += $2 } END { print n + 0 }' "$work/shapes")"
if [ "$LIST" -eq 1 ]; then
    sort -k2,2nr -k1,1 "$work/shapes"
fi
