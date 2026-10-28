#!/usr/bin/env bash
# Frontend cost: callgrind Ir of the frontend (parse + build + lower) for the budget cases of
# ADR-15334 section 3.2, against the frozen pre-flip baseline.
#
#   scripts/frontend-cost.sh measure [BIN]       # frontend Ir per case, ratio to baseline
#   scripts/frontend-cost.sh check [BIN]         # as measure; exit 1 if a case exceeds 110%
#   scripts/frontend-cost.sh build-baseline      # build the baseline commit (profiling) into tmp/
#   scripts/frontend-cost.sh record-baseline     # measure the baseline build, rewrite the data file
#
# BIN defaults to target/profiling/mutsu (`cargo build --profile profiling`).
#
# The figure is "frontend Ir": callgrind collects only inside `parse_compilation_unit_of`, from
# its call to the return of the lowered statements, so startup and the compiler are excluded.
# The whole-process `-c` Ir is printed beside it as context and is never gated. Each case runs
# twice and the second run is quoted (the first recompiles the module precompilation cache,
# see .agents/skills/perf-tuning/SKILL.md section 0).
#
# The baseline is the parent of the commit that made the RakuAST round trip the default
# (e1463839, ADR-10723 section 6.122): the "ordinary frontend" of ADR-10723 section 2.4. It is
# built with a one-line patch (`#[inline(never)]` on the toggled function, which `main` carries).
# The bound is cumulative and absolute: every case at most 110% of that baseline.
set -euo pipefail

cd "$(dirname "$0")/.."

DATA=scripts/frontend-cost-baseline.txt
BASELINE_DIR=tmp/frontend-cost-baseline
BOUND_PERCENT=110
TOGGLE='*parse_compilation_unit_of*'

# name<TAB>path. The four cases of ADR-10723 section 6.109 plus a declaration-heavy test, so a
# program that is mostly declarations (the families the round trip reconstructs) is measured.
CASES=(
    "zef-cli	vendor/zef/lib/Zef/CLI.rakumod"
    "s32-exceptions-misc	roast/S32-exceptions/misc.t"
    "s12-class-attributes	roast/S12-class/attributes.t"
    "s15-nfg-emoji-test	roast/S15-nfg/emoji-test.t"
    "grammar-cursor-attributes	t/grammar/grammar-cursor-attributes.t"
)

# measure_one BIN FILE -> "<frontend Ir> <whole-process Ir>"
measure_one() {
    local bin=$1 file=$2 out ir
    out=$(mktemp)
    # Warm: the first run may recompile precompiled modules.
    timeout 600 "$bin" -c "$file" >/dev/null 2>&1 || true
    MUTSU_FUDGE=1 timeout 1800 valgrind --tool=callgrind --cache-sim=no --branch-sim=no \
        --toggle-collect="$TOGGLE" --callgrind-out-file="$out" \
        "$bin" -c "$file" >/dev/null 2>"$out.err" || true
    local collected total
    collected=$(sed -n 's/^==[0-9]*== Collected : *\([0-9]*\).*/\1/p' "$out.err" | tail -1)
    total=$(sed -n 's/^==[0-9]*== *Events *: .*//p;s/^==[0-9]*== I *refs: *\([0-9,]*\).*/\1/p' "$out.err" \
        | tail -1 | tr -d ',')
    rm -f "$out" "$out.err"
    if [ -z "$collected" ]; then
        echo "frontend-cost: no 'Collected' line for $file (is $bin a profiling build?)" >&2
        return 1
    fi
    echo "$collected ${total:-0}"
}

measure_all() {
    local bin=$1 name path
    for c in "${CASES[@]}"; do
        name=${c%%$'\t'*}
        path=${c#*$'\t'}
        read -r front whole < <(measure_one "$bin" "$path")
        printf '%s\t%s\t%s\n' "$name" "$front" "$whole"
    done
}

baseline_sha() { sed -n 's/^sha[[:space:]]*//p' "$DATA"; }

baseline_ir() { awk -F'\t' -v n="$1" '$1==n {print $2}' "$DATA"; }

cmd=${1:-measure}
case "$cmd" in
build-baseline)
    sha=$(git rev-parse --verify 'e1463839^' 2>/dev/null) || {
        git fetch origin e1463839 --depth=2 || git fetch --shallow-since=2026-10-26 origin main
        sha=$(git rev-parse --verify 'e1463839^')
    }
    if [ ! -d "$BASELINE_DIR" ]; then
        git worktree add --detach "$BASELINE_DIR" "$sha"
    fi
    (
        cd "$BASELINE_DIR"
        git checkout -q --detach "$sha"
        # The toggled function must be a symbol; `main` carries this attribute, the baseline does not.
        if ! grep -q 'inline(never)' src/parse_dispatch.rs; then
            sed -i 's|^pub(crate) fn parse_compilation_unit_of(|#[inline(never)]\n&|' src/parse_dispatch.rs
        fi
        CARGO_TARGET_DIR="$PWD/target" cargo build --profile profiling
    )
    echo "baseline $sha built: $BASELINE_DIR/target/profiling/mutsu"
    ;;
record-baseline)
    bin=$BASELINE_DIR/target/profiling/mutsu
    [ -x "$bin" ] || { echo "run build-baseline first" >&2; exit 2; }
    sha=$(git -C "$BASELINE_DIR" rev-parse HEAD)
    {
        printf 'sha\t%s\n' "$sha"
        printf '# case\tfrontend_ir\twhole_process_ir  (callgrind, profiling build, warm, mutsu -c)\n'
        measure_all "$PWD/$bin"
    } >"$DATA.new"
    mv "$DATA.new" "$DATA"
    cat "$DATA"
    ;;
measure | check)
    bin=${2:-target/profiling/mutsu}
    [ -x "$bin" ] || { echo "no $bin (cargo build --profile profiling)" >&2; exit 2; }
    printf 'baseline %s\n' "$(baseline_sha)"
    printf '%-28s %16s %16s %8s\n' case frontend-Ir baseline-Ir ratio
    worst=0
    fail=0
    while IFS=$'\t' read -r name front whole; do
        base=$(baseline_ir "$name")
        pct=$(awk -v a="$front" -v b="$base" 'BEGIN { printf "%.1f", 100 * a / b }')
        printf '%-28s %16s %16s %7s%%   (whole -c Ir %s)\n' "$name" "$front" "$base" "$pct" "$whole"
        if awk -v p="$pct" -v m="$BOUND_PERCENT" 'BEGIN { exit !(p > m) }'; then fail=1; fi
    done < <(measure_all "$bin")
    if [ "$fail" = 1 ]; then
        echo "frontend-cost: a case is above ${BOUND_PERCENT}% of the baseline (ADR-15334 section 3.2)" >&2
        [ "$cmd" = check ] && exit 1
    fi
    ;;
*)
    sed -n '2,12p' "$0"
    exit 2
    ;;
esac
