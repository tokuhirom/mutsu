#!/usr/bin/env bash
# Call-path measurement harness (ADR-15737, slice 1).
#
# Prints one TSV row per input, the same four numbers every slice of the
# ADR quotes in its PR body:
#
#   input  Ir/iter  allocs/iter  I1miss%  mispredict%  ns/iter(wall)
#
# Every figure is a SLOPE between two iteration counts (CALLPATH_N=N1 and N2),
# so process startup, module load and first-call warm-up cancel out. Ir and
# allocations come from one callgrind run per count; the I1 miss rate and the
# branch mispredict rate come from the same run's simulated caches (callgrind
# with --cache-sim=yes --branch-sim=yes), so the three deterministic numbers
# share one profile. Wall time is user CPU of a native run at two larger counts,
# the minimum of three (this box's wall clock drifts; quote it only beside a
# baseline taken minutes apart, perf-tuning skill section 0).
#
# Usage:
#
#   cargo build --profile profiling
#   scripts/call-path-ab.sh                              # every input below
#   scripts/call-path-ab.sh plain-call mooish-lazy
#   MUTSU_BIN=target/release/mutsu scripts/call-path-ab.sh plain-call
#
# A/B (perf-tuning skill section 0): build and measure the baseline from
# current origin/main first, then your change; never reuse an older "before".
#
# Inputs live in benchmarks/call-path/<name>.raku and read CALLPATH_N. The
# mooish-lazy input needs the real AttrX::Mooish 1.0.10, which is fetched once
# into tmp/call-path/mooish (no vendoring: it is an ecosystem dist).
set -euo pipefail

MUTSU=${MUTSU_BIN:-target/profiling/mutsu}
TIMEOUT=${CALLPATH_TIMEOUT:-900}
OUT=${CALLPATH_CG_OUT:-tmp/call-path/cg.out}
MOOISH_DIR=tmp/call-path/mooish
MOOISH_URL=https://360.zef.pm/A/TT/ATTRX_MOOISH/9f8227d0681bf87ccfb0771db4f35dd86e0446f6.tar.gz

# Per-input callgrind counts (N1 N2) and native wall counts (W1 W2).
counts() { # $1=input -> "N1 N2 W1 W2"
    case "$1" in
        plain-call)  echo "100 300 20000 60000" ;;
        mooish-lazy) echo "10 50 100 300" ;;
        *)           echo "${CALLPATH_COUNTS:-100 300 2000 6000}" ;;
    esac
}
extra_args() { # $1=input
    case "$1" in
        mooish-lazy) echo "-I $MOOISH_DIR/lib" ;;
        *)           echo "" ;;
    esac
}

if ! command -v valgrind >/dev/null 2>&1; then
    echo "call-path-ab: valgrind not found (apt-get install -y valgrind)" >&2
    exit 1
fi
[ -x "$MUTSU" ] || { echo "call-path-ab: $MUTSU not built" >&2; exit 1; }

ALLOC_AWK=$(scripts/bench-det.sh --alloc-awk)
mkdir -p "$(dirname "$OUT")"

fetch_mooish() {
    [ -d "$MOOISH_DIR/lib" ] && return
    local tgz=tmp/call-path/mooish.tgz
    echo "call-path-ab: fetching AttrX::Mooish 1.0.10" >&2
    curl -fsS -m 120 -o "$tgz" "$MOOISH_URL"
    mkdir -p "$MOOISH_DIR"
    tar xzf "$tgz" -C "$MOOISH_DIR" --strip-components=1
}

# first_num <label-regex>: the first integer after the label on a valgrind summary line.
sim() { # $1=stderr-file $2=label
    sed -n "s/^==[0-9]*== *$2: *\([0-9,]*\).*/\1/p" "$1" | head -1 | tr -d ','
}

run_cg() { # $1=input $2=n; sets IR ALLOCS I1M BR MISP
    local input=$1 n=$2 err=tmp/call-path/cg.err
    rm -f "$OUT"
    # shellcheck disable=SC2046
    CALLPATH_N=$n timeout "$TIMEOUT" valgrind --tool=callgrind --callgrind-out-file="$OUT" \
        --cache-sim=yes --branch-sim=yes \
        "$MUTSU" $(extra_args "$input") "benchmarks/call-path/$input.raku" >/dev/null 2>"$err"
    IR=$(sim "$err" "I *refs")
    I1M=$(sim "$err" "I1 *misses")
    BR=$(sim "$err" "Branches")
    MISP=$(sim "$err" "Mispredicts")
    ALLOCS=$(awk "$ALLOC_AWK" "$OUT")
}

wall_user() { # $1=input $2=n -> minimum user seconds of three native runs
    local input=$1 n=$2 best="" t
    for _ in 1 2 3; do
        # shellcheck disable=SC2046
        t=$( { TIMEFORMAT=%U; time CALLPATH_N=$n "$MUTSU" $(extra_args "$input") \
              "benchmarks/call-path/$input.raku" >/dev/null 2>&1; } 2>&1 )
        if [ -z "$best" ] || awk "BEGIN{exit !($t < $best)}"; then best=$t; fi
    done
    echo "$best"
}

inputs=("$@")
if [ "${#inputs[@]}" -eq 0 ]; then
    inputs=(plain-call mooish-lazy)
fi

printf 'input\tIr/iter\tallocs/iter\tI1miss%%\tmispredict%%\tns/iter\n'
for input in "${inputs[@]}"; do
    [ "$input" = mooish-lazy ] && fetch_mooish
    read -r n1 n2 w1 w2 <<<"$(counts "$input")"
    # Warm the module precompilation cache (perf-tuning skill section 0): the
    # first run after a build recompiles the module under test.
    # shellcheck disable=SC2046
    CALLPATH_N=1 "$MUTSU" $(extra_args "$input") "benchmarks/call-path/$input.raku" >/dev/null 2>&1 || true
    # shellcheck disable=SC2046
    CALLPATH_N=1 "$MUTSU" $(extra_args "$input") "benchmarks/call-path/$input.raku" >/dev/null 2>&1 || true
    run_cg "$input" "$n1"; ir1=$IR; al1=$ALLOCS; im1=$I1M; br1=$BR; mp1=$MISP
    run_cg "$input" "$n2"; ir2=$IR; al2=$ALLOCS; im2=$I1M; br2=$BR; mp2=$MISP
    dn=$((n2 - n1))
    t1=$(wall_user "$input" "$w1")
    t2=$(wall_user "$input" "$w2")
    awk -v name="$input" -v dn="$dn" -v dw="$((w2 - w1))" \
        -v ir1="$ir1" -v ir2="$ir2" -v al1="$al1" -v al2="$al2" \
        -v im1="$im1" -v im2="$im2" -v br1="$br1" -v br2="$br2" \
        -v mp1="$mp1" -v mp2="$mp2" -v t1="$t1" -v t2="$t2" 'BEGIN {
        dir = ir2 - ir1
        printf "%s\t%.0f\t%.1f\t%.3f\t%.2f\t%.0f\n", name, dir / dn, (al2 - al1) / dn,
               100 * (im2 - im1) / dir, 100 * (mp2 - mp1) / (br2 - br1),
               (t2 - t1) * 1e9 / dw }'
done
