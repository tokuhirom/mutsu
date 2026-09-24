#!/usr/bin/env bash
# Measure the growth order of Array / List / Seq operations empirically.
#
# Each case is timed inside mutsu (`now`) at N and at 2N and the ratio of the
# two times is printed:
#
#   ratio ~1  -> the timed body is O(1) in N
#   ratio ~2  -> linear in N
#   ratio ~4  -> quadratic in N
#
# Most deficit cases time a FIXED number of calls against an array of N
# elements, so a ratio of ~2 there already means "O(e) per call" where Rakudo
# is O(1); the `expect` column says which ratio is healthy for each case.
# Cases whose ratio exceeds their expectation by 1.5x are flagged. The
# annotations these cases back are the `Rakudo: O(..) -- see #NNNN` comments;
# rules in docs/complexity-annotations.md.
#
# A manual diagnostic, NOT a CI gate: wall-clock ratios move with load. Use a
# release build and re-run a case before trusting it.
#
# usage: scripts/array-complexity-check.sh [case-name-substring ...]
#   MUTSU_BIN   binary to measure (default target/release/mutsu)
#   SCALE       multiply every case's base N (default 1)

set -u
BIN=${MUTSU_BIN:-target/release/mutsu}
SCALE=${SCALE:-1}
FILTERS=("$@")

# name | base N | expected healthy ratio | setup | timed body
#   NN in setup/body is replaced by the size; the body runs once (put any
#   loop in the body itself).
CASES=(
    # --- container / mutation ---------------------------------------------
    'push (control)|200000|2|my @a;|@a.push($_) for ^NN'
    'pop (control)|200000|2|my @a = ^NN;|@a.pop while @a'
    'index read/write (control)|200000|2|my @a = ^NN; my $x;|for ^NN { $x = @a[$_]; @a[$_] = $x }'
    'hash-elem push (control)|100000|2|my %h;|%h<k>.push($_) for ^NN'
    'shift loop|100000|2|my @a = ^NN;|@a.shift while @a'
    'unshift loop|100000|2|my @a;|@a.unshift($_) for ^NN'
    'queue push+shift (2000 steps)|50000|1|my @q = ^NN;|for ^2000 { @q.push($_); @q.shift }'
    'prepend many (one call)|40000|2|my @a = ^NN;|@a.prepend(^NN)'
    'hash-elem unshift|40000|2|my %h; %h<k> = [];|%h<k>.unshift($_) for ^NN'
    'splice insert at front (one call)|40000|2|my @a = ^NN; my @b = ^NN;|@a.splice(0, 0, @b)'
    'splice(0,1) loop|50000|2|my @a = ^NN;|@a.splice(0, 1) while @a'
    'ASSIGN-POS loop|10000|2|my @a = ^NN;|@a.ASSIGN-POS($_, 1) for ^NN'
    'shaped array write loop|10000|2|my @a[NN];|@a[$_] = $_ for ^NN'
    'List.List (2000 calls)|50000|1|my $l = (^NN).List;|$l.List for ^2000'
    'native int shift loop|50000|2|my int @a = ^NN;|@a.shift while @a'
    # --- traversal / transform ----------------------------------------------
    'map(...).head(3) (10 calls)|200000|1|my @a = ^NN;|for ^10 { @a.map(* + 1).head(3).List }'
    '[~] @a|20000|2|my @a = ^NN;|my $s = [~] @a'
    '.reduce(&[~])|20000|2|my @a = ^NN;|my $s = @a.reduce(&[~])'
    'unique of Rats|4000|2|my @a = (^NN).map({ $_ / 7 });|@a.unique'
    'unique of Ints (control)|200000|2|my @a = ^NN;|@a.unique'
    'tail(3) (100 calls)|200000|1|my @a = ^NN;|for ^100 { @a.tail(3) }'
    'skip (100 calls)|200000|1|my @a = ^NN;|for ^100 { @a.skip(NN - 2).List }'
    'for @a { last } (1000 calls)|200000|1|my @a = ^NN;|for ^1000 { for @a { last } }'
    'head(3) (1000 calls, control)|200000|1|my @a = ^NN;|for ^1000 { @a.head(3) }'
    'sort with key (control)|100000|2|my @a = (^NN).map({ ($_ * 7919) % NN });|@a.sort({ $_ % 97 })'
    'join (control)|400000|2|my @a = ^NN;|@a.join(",")'
    # --- search / random / stringify ----------------------------------------
    'first(* == 0) (1000 calls)|20000|1|my @a = ^NN;|for ^1000 { @a.first(* == 0) }'
    'pick (1000 calls)|100000|1|my @a = ^NN;|for ^1000 { @a.pick }'
    'combinations(2).head(10)|500|1|my @s = ^NN;|@s.combinations(2).head(10)'
    'eqv, lengths differ (1000 calls)|100000|1|my @a = ^NN; my @c = ^(NN + 1);|for ^1000 { @a eqv @c }'
    '@a == @b (1000 calls)|20000|1|my @a = ^NN; my @b = ^NN;|for ^1000 { @a == @b }'
    '.gist (100 calls)|100000|1|my @a = ^NN;|for ^100 { @a.gist }'
    '.Set (control)|200000|2|my @a = ^NN;|@a.Set'
    '.pick(*) (control)|200000|2|my @a = ^NN;|@a.pick(*)'
)

time_case() {
    local setup=$1 body=$2 n=$3
    setup=${setup//NN/$n}
    body=${body//NN/$n}
    timeout 300 "$BIN" -e "$setup my \$t0 = now; $body; say now - \$t0;" 2>/dev/null | tail -1
}

printf '%-36s %8s %10s %10s %6s %s\n' case N 't(N)' 't(2N)' expect ratio
for c in "${CASES[@]}"; do
    IFS='|' read -r name base expect setup body <<<"$c"
    if [ ${#FILTERS[@]} -gt 0 ]; then
        hit=0
        for f in "${FILTERS[@]}"; do
            [[ $name == *"$f"* ]] && hit=1
        done
        [ $hit = 1 ] || continue
    fi
    n=$((base * SCALE))
    t1=$(time_case "$setup" "$body" "$n")
    t2=$(time_case "$setup" "$body" $((n * 2)))
    verdict=$(awk -v a="$t1" -v b="$t2" -v e="$expect" 'BEGIN {
        if (a == "" || b == "" || a + 0 <= 0) { print "? (error/timeout)"; exit }
        r = b / a; printf "%.2f %s", r, (r >= 1.5 * e ? "SUPERLINEAR" : "ok") }')
    printf '%-36s %8d %10.4f %10.4f %6s %s\n' "$name" "$n" "${t1:-0}" "${t2:-0}" "$expect" "$verdict"
done
