#!/usr/bin/env bash
# Measure the growth order of individual `nqp::` ops empirically.
#
# Each case is a setup snippet plus a loop that runs the op N times. The loop
# is timed inside mutsu (`now`), once at N and once at 2N, and the ratio of
# the two times is printed:
#
#   ratio ~2  -> the loop is O(N)   (each op O(1) amortized)
#   ratio ~4  -> the loop is O(N^2) (each op O(N))
#
# Cases flagged SUPERLINEAR (ratio >= 3) correspond to the
# `MoarVM: O(..) -- see #NNNN` annotations in src/runtime/nqp_ops*.rs; the
# case name is the op name used there. The legend for those annotations is in
# src/runtime/nqp_op_ids.rs ("Complexity annotations").
#
# This is a manual diagnostic, NOT a CI gate: wall-clock ratios move with
# machine load. Use a release build, and re-run a case before trusting it.
#
# usage: scripts/nqp-complexity-check.sh [-n N] [case-name-substring ...]
#   MUTSU_BIN   binary to measure (default target/release/mutsu)

set -u
BIN=${MUTSU_BIN:-target/release/mutsu}
N=10000
if [ "${1:-}" = "-n" ]; then
    N=$2
    shift 2
fi
FILTERS=("$@")

# name | setup (NN = size) | loop body executed for $i in ^NN
CASES=(
    'chars|my $s = "a" x NN;|nqp::chars($s)'
    'ordat (same string)|my $s = "a" x NN;|nqp::ordat($s, $i)'
    'ordat (two strings)|my $s = "a" x NN; my $u = "b" x NN;|nqp::ordat($s, $i); nqp::ordat($u, $i)'
    'substr (two strings)|my $s = "a" x NN; my $u = "b" x NN;|nqp::substr($s, $i, 1); nqp::substr($u, $i, 1)'
    'index (two strings)|my $s = "a" x NN; my $u = "b" x NN;|nqp::index($s, "a", $i); nqp::index($u, "b", $i)'
    'iscclass (two strings)|my $s = "a" x NN; my $u = "b" x NN;|nqp::iscclass(nqp::const::CCLASS_ALPHABETIC, $s, $i); nqp::iscclass(nqp::const::CCLASS_ALPHABETIC, $u, $i)'
    'eqat (two strings)|my $s = "a" x NN; my $u = "b" x NN;|nqp::eqat($s, "a", $i); nqp::eqat($u, "b", $i)'
    'radix|my $s = "1 " x NN;|nqp::radix(10, $s, 2 * $i, 0)'
    'push|my $l := nqp::list();|nqp::push($l, $i)'
    'shift|my $l := nqp::list(); nqp::push($l, $_) for ^NN;|nqp::shift($l)'
    'unshift|my $l := nqp::list();|nqp::unshift($l, $i)'
    'atpos|my $l := nqp::list(); nqp::push($l, $_) for ^NN;|nqp::atpos($l, $i)'
    'bindpos_i (buf8)|my $b := buf8.new;|nqp::bindpos_i($b, $i, 1)'
    'atpos_i (buf8)|my $b := buf8.new(0 xx NN);|nqp::atpos_i($b, $i)'
    'atpos_i (buf32)|my $b := buf32.new(258 xx NN);|nqp::atpos_i($b, $i)'
    'writeuint|my $b := buf8.new;|nqp::writeuint($b, $i, 7, 0)'
    'readuint|my $b := buf8.new(0 xx NN);|nqp::readuint($b, $i, 0)'
    'slice|my $b := buf8.new(0 xx NN);|nqp::slice($b, $i, $i)'
    'splice (buf8 append)|my $b := buf8.new; my $one := buf8.new(1);|nqp::splice($b, $one, nqp::elems($b), 0)'
)

time_case() {
    local setup=$1 body=$2 n=$3
    local code="use nqp; ${setup//NN/$n} my \$t0 = now; for ^$n -> \$i { $body }; say now - \$t0;"
    timeout 300 "$BIN" -e "$code" 2>/dev/null | tail -1
}

printf '%-24s %12s %12s %7s\n' case "t(N=$N)" "t(N=$((N * 2)))" ratio
for c in "${CASES[@]}"; do
    IFS='|' read -r name setup body <<<"$c"
    if [ ${#FILTERS[@]} -gt 0 ]; then
        hit=0
        for f in "${FILTERS[@]}"; do
            [[ $name == *"$f"* ]] && hit=1
        done
        [ $hit = 1 ] || continue
    fi
    t1=$(time_case "$setup" "$body" "$N")
    t2=$(time_case "$setup" "$body" $((N * 2)))
    verdict=$(awk -v a="$t1" -v b="$t2" 'BEGIN {
        if (a == "" || b == "" || a + 0 <= 0) { print "? (error/timeout)"; exit }
        r = b / a; printf "%.2f %s", r, (r >= 3 ? "SUPERLINEAR" : "ok") }')
    printf '%-24s %12.4f %12.4f %s\n' "$name" "${t1:-0}" "${t2:-0}" "$verdict"
done
