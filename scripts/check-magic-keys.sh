#!/usr/bin/env bash
# Ratchet on hand-built `__mutsu_*` metadata env keys (issue #8087).
#
# `format!("__mutsu_<ns>::{name}")` followed by an `Env` probe has been the
# profiling finding in five separate perf campaigns (#7571, #7766, the two
# bench-ctor rounds, and #8069's element store at 22 interns and 8 heap
# allocations per `@a[$i] = $v`). Each was fixed by memoizing the one key that
# profile happened to walk through, and the pattern grew back, because nothing
# stopped the next site being written.
#
# `MetaNs` (src/runtime/meta_ns.rs) is the memoizing constructor that should
# build every one of these keys. This script is what keeps the sites that have
# NOT moved to it from multiplying: it counts the hand-built sites per file and
# compares against a checked-in baseline. A count may go DOWN (that is the
# work) or stay the same; it may never go up, and a file not in the baseline may
# not introduce one at all.
#
# So this is a ratchet, not a ban. The debt stays visible and shrinking, and
# nobody has to know a site is hot before they are told not to add another.
#
#   scripts/check-magic-keys.sh              # check against the baseline
#   scripts/check-magic-keys.sh --update     # re-baseline after converting sites
#
# `make check-magic-keys` runs the check, and it is a `make test` prerequisite.
#
# The endgame is that the baseline reaches zero and these keys stop existing
# altogether, because the metadata moves onto the binding's own descriptor
# (#8069 §4.1) or off the per-frame env (#7817 / ADR-0084). Until then, down
# only.
set -euo pipefail

cd "$(dirname "$0")/.."

BASELINE=scripts/magic-keys-baseline.tsv

# One hand-built key site = a `format!` whose literal starts a `__mutsu_`
# namespace. Deliberately narrow: it matches construction, not the many places
# that legitimately MENTION a key (comments, `starts_with` probes, tests).
#
# src/runtime/meta_ns.rs is exempt -- it is the constructor those sites are
# supposed to be using, and its own `format!` is the one that is allowed.
count_sites() {
    grep -rn 'format!("__mutsu_' src/ --include='*.rs' \
        | grep -v '^src/runtime/meta_ns.rs:' \
        | cut -d: -f1 | sort | uniq -c \
        | awk '{printf "%s\t%s\n", $2, $1}' | LC_ALL=C sort
}

if [ "${1:-}" = "--update" ]; then
    {
        echo "# Hand-built __mutsu_* key sites per file. Ratcheted by"
        echo "# scripts/check-magic-keys.sh: these counts may go down, never up."
        echo "# Regenerate with: scripts/check-magic-keys.sh --update"
        count_sites
    } > "$BASELINE"
    echo "check-magic-keys: baseline updated ($(grep -vc '^#' "$BASELINE") files, $(count_sites | awk -F'\t' '{n+=$2} END {print n+0}') sites)"
    exit 0
fi

if [ ! -f "$BASELINE" ]; then
    echo "check-magic-keys: $BASELINE missing; run scripts/check-magic-keys.sh --update" >&2
    exit 1
fi

current=$(mktemp)
trap 'rm -f "$current"' EXIT
count_sites > "$current"

fail=0
total_now=0
total_base=0

while IFS=$'\t' read -r file n; do
    total_now=$((total_now + n))
    base=$(awk -F'\t' -v f="$file" '$1==f {print $2}' "$BASELINE")
    if [ -z "$base" ]; then
        echo "check-magic-keys: $file introduces $n hand-built __mutsu_* key(s)." >&2
        echo "  Build them with MetaNs::<Namespace>.key(sym) instead (src/runtime/meta_ns.rs)," >&2
        echo "  and probe the env with get_sym / contains_key_sym. See #8087." >&2
        fail=1
    elif [ "$n" -gt "$base" ]; then
        echo "check-magic-keys: $file went from $base to $n hand-built __mutsu_* key(s)." >&2
        echo "  This count is a ratchet and may only go down. Use MetaNs; see #8087." >&2
        fail=1
    fi
done < "$current"

# A file whose sites are all gone drops out of the current list entirely; that
# is progress, but leaving it in the baseline lets it silently grow back to the
# old number later. Report it so the baseline is re-cut.
while IFS=$'\t' read -r file n; do
    case "$file" in \#*) continue ;; esac
    total_base=$((total_base + n))
    now=$(awk -F'\t' -v f="$file" '$1==f {print $2}' "$current")
    if [ -z "$now" ] || [ "$now" -lt "$n" ]; then
        echo "check-magic-keys: $file is now at ${now:-0} (baseline $n) — re-cut with --update." >&2
        fail=1
    fi
done < "$BASELINE"

if [ "$fail" = 1 ]; then
    echo "check-magic-keys: FAILED ($total_now sites now, baseline $total_base)" >&2
    exit 1
fi
echo "check-magic-keys: ok ($total_now hand-built __mutsu_* key sites, baseline $total_base)"
