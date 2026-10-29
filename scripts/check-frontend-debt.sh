#!/usr/bin/env bash
# Ratchet for the frontend debt ADR-15334 retires (section 3.3). Fails when a count rises above
# its ceiling; a PR that lowers a count lowers the ceiling in the same change, and a PR that does
# not touch a count never edits its file (a drop must not require a re-cut, so a count below its
# ceiling only prints a reminder). Ceilings live one metric per file in scripts/frontend-debt.d/.
#
#   synthetic-block-sites   lines in src/parser naming `SyntheticBlock` (producers and matchers)
#   internal-name-literals  distinct `"__...` literals in src/parser
#   convert-lines           lines of src/rakuast/convert.rs
#
# Slice 9 deletes this check together with the last thing it counts.
set -euo pipefail

cd "$(dirname "$0")/.."
D=${FRONTEND_DEBT_DIR:-scripts/frontend-debt.d}

count_synthetic_block_sites() { grep -rn 'SyntheticBlock' src/parser | wc -l; }
count_internal_name_literals() { { grep -rhoE '"__[A-Za-z0-9_]+' src/parser || true; } | sort -u | wc -l; }
count_convert_lines() { wc -l <src/rakuast/convert.rs; }

# check_metric NAME COUNT -> 0 ok, 1 above ceiling
check_metric() {
    local name=$1 count=$2 ceiling
    ceiling=$(tr -d '[:space:]' <"$D/$name.txt")
    if [ "$count" -gt "$ceiling" ]; then
        echo "check-frontend-debt: $name is $count, above its ceiling $ceiling ($D/$name.txt)" >&2
        echo "  ADR-15334 retires this debt; a change must not add to it." >&2
        return 1
    fi
    if [ "$count" -lt "$ceiling" ]; then
        echo "check-frontend-debt: $name is $count, below its ceiling $ceiling: lower $D/$name.txt to $count"
    fi
}

if [ "${1:-}" = --self-test ]; then
    t=$(mktemp -d)
    trap 'rm -rf "$t"' EXIT
    D=$t
    echo 5 >"$t/m.txt"
    check_metric m 5 >/dev/null
    check_metric m 4 | grep -q 'lower' || { echo "self-test: no reminder below the ceiling" >&2; exit 1; }
    if check_metric m 6 2>/dev/null; then echo "self-test: a rise passed" >&2; exit 1; fi
    echo "check-frontend-debt self-test: ok"
    exit 0
fi

rc=0
check_metric synthetic-block-sites "$(count_synthetic_block_sites)" || rc=1
check_metric internal-name-literals "$(count_internal_name_literals)" || rc=1
check_metric convert-lines "$(count_convert_lines)" || rc=1
exit $rc
