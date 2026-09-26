#!/usr/bin/env bash
# Measure the growth order of individual VM opcodes empirically.
#
# Each case runs a FIXED timed body and grows something the opcode should
# not care about (or should care about linearly) from N to 2N: the number of
# locals in the frame, env entries, registered routines, MRO depth, call
# depth, container size. The body is timed inside mutsu (`now`), so setup and
# compilation are excluded. The ratio t(2N)/t(N) is printed next to the
# ratio a healthy implementation shows (`expect`); a case is flagged when it
# exceeds 1.5x that.
#
# Placeholders in setup/body:
#   NN      the size
#   LOCALS  NN generated `my $lvI = I;` declarations (a big frame)
#   CLASSES a linear chain of NN classes C0..C<NN> (C0 has `method m {1}`)
#   STMTS   NN copies of `$s++;` (a big block that declares no locals)
#
# The cases back the `Rakudo: O(..) -- see #NNNN` annotations on the
# `exec_one_dispatch` arms (src/vm/vm_exec_dispatch.rs) and their handlers;
# rules in docs/complexity-annotations.md. A manual diagnostic, NOT a CI
# gate: wall-clock ratios move with load. Use a release build.
#
# usage: scripts/vm-complexity-check.sh [case-name-substring ...]
#   MUTSU_BIN   binary to measure (default target/release/mutsu)
#   SCALE       multiply every case's base N (default 1)

set -u
BIN=${MUTSU_BIN:-target/release/mutsu}
SCALE=${SCALE:-1}
FILTERS=("$@")
TMPDIR_CX=$(mktemp -d "${TMPDIR:-/tmp}/vmcx.XXXXXX")
trap 'rm -rf "$TMPDIR_CX"' EXIT

# name | base N | expected healthy ratio | setup | timed body
CASES=(
    # --- per-op cost that should not depend on the frame's size -------------
    'say vs frame locals|500|1|LOCALS|for ^2000 { say 1 }'
    'numeric == vs frame locals (control)|500|1|LOCALS my $x = 42; my $r = 0;|for ^100000 { $r++ if $x == 42 }'
    '~~ Int vs frame locals|250|1|LOCALS my $x = 42; my $r = 0;|for ^20000 { $r++ if $x ~~ Int }'
    '~~ code-bearing regex vs frame locals|250|1|LOCALS my $x = "ab"; my $w = "b"; my $r = 0;|for ^5000 { $r++ if $x ~~ / (.) <?{ $0 eq $w }> / }'
    '~~ s/// vs frame locals|250|1|LOCALS my $w = "Q"; my $r = 0;|for ^5000 { my $x = "ab"; $x ~~ s/b/$w/; $r++ }'
    '~~ junction of regexes vs frame locals|250|1|LOCALS my $w = "b"; my $r = 0;|for ^5000 { $r++ if "ab" ~~ any(/zz/, /a$w/) }'
    '~~ regex with EVAL in code vs frame locals|1000|1|use MONKEY-SEE-NO-EVAL; LOCALS my $x = "ab"; my $w = "b"; my $r = 0;|for ^300 { $r++ if $x ~~ / (.) <?{ $0 eq EVAL(q[$w]) }> / }'
    '~~ regex with ::($n) in code vs frame locals|250|1|LOCALS my $x = "ab"; my $w = "b"; my $n = q[$w]; my $r = 0;|for ^5000 { $r++ if $x ~~ / (.) <?{ $0 eq ::($n) }> / }'
    # The Proxy case's residual growth is the FETCH itself, not ~~ -- see #9385.
    '~~ Proxy RHS vs frame locals|250|1|my $re = /b/; my $p := Proxy.new(FETCH => -> $ { $re }, STORE => -> $, $ { }); LOCALS my $r = 0;|for ^5000 { $r++ if "ab" ~~ $p }'
    '~~ lazy list RHS vs frame locals|250|1|my @l = lazy (/zz/, /b/); LOCALS my $r = 0;|for ^5000 { $r++ if "ab" ~~ @l }'
    '$outer = $_ (SetGlobal) vs env|2000|1|LOCALS my $s = 0;|for ^20000 { $s += (my $z = $_) }'
    'bare block { my } vs env|500|1|LOCALS my $t = 0;|for ^5000 { { my $y = 1; $t += $y } }'
    '"a{ $t }b" vs env|1000|1|LOCALS my $t = 1; my $s;|for ^5000 { $s = "a{ $t }b" }'
    '"a$t b" vs env (control)|1000|1|LOCALS my $t = 1; my $s;|for ^5000 { $s = "a$t b" }'
    '&f read vs frame locals|500|1|sub f { 1 }; LOCALS my $c;|for ^20000 { $c = &f }'
    '-> { $q } creation vs env|2000|1|LOCALS my $c;|for ^20000 { my $q = $_; $c = -> { $q } }'
    'sub returning -> { } vs env|2000|1|sub mk { my $q = 1; -> { $q } }; LOCALS my $s = 0;|for ^20000 { $s += mk()() }'
    'gather { take } vs env|2000|1|LOCALS my $s = 0;|for ^20000 { my @g = gather { take 1 }; $s += @g[0] }'
    'eager gather vs frame locals|2000|1|LOCALS my $s = 0;|for ^5000 { $s += (eager gather { take 1 })[0] }'
    'but True vs frame locals|500|1|LOCALS|for ^20000 { my $y = 5 but True }'
    'does R vs frame locals|500|1|role R { }; LOCALS|for ^5000 { my $y = 5; $y does R }'
    'andthen (user .defined) vs frame locals|500|1|class O { method defined { True } }; my $o = O.new; LOCALS my $s = 0;|for ^20000 { $s += ($o andthen 1) }'
    'user sink vs frame locals|500|1|class K { method sink { } }; my $k = K.new; LOCALS|for ^20000 { $k.self }'
    'try { CATCH .resume } vs code size|500|1|LOCALS|for ^2000 { try { CATCH { default { .resume } }; 1 } }'
    'try { } vs code size (control)|500|1|LOCALS|for ^2000 { try { 1 } }'
    'sub with NN my, called (SetVarDynamic)|200|2|my $src = "sub f \{ " ~ (^NN).map({ "my \$a$_ = $_;" }).join ~ " 1 \}"; use MONKEY-SEE-NO-EVAL; my &f = EVAL $src; my $t = 0;|for ^2000 { $t += f() }'
    'P::<$p1> vs package size|250|1|use MONKEY-SEE-NO-EVAL; EVAL "package P \{ " ~ (^NN).map({ "our \$p$_ = $_;" }).join ~ " \}"; my $s;|for ^1000 { $s = P::<$p1> }'
    # --- per-op cost vs the size of its operand / the program ---------------
    '?@a.grep (Bool of a Seq)|10000|1|my @a = ^NN; my $s;|for ^200 { $s = ?@a.grep(* >= 0) }'
    '@a === @a|20000|1|my @a = ^NN; my $r = 0;|for ^2000 { $r++ if @a === @a }'
    # The recursion to depth NN is itself O(NN); enough throws keep it a small
    # share of the body, so a flat per-throw cost reads as a ratio near 1.
    'die at call depth NN|1000|1|sub r($n) { if $n == 0 { for ^20000 { try { die "x" } }; return 0 }; 1 + r($n - 1) };|r(NN)'
    'method call vs MRO depth|40|1|CLASSES our @objs = C''NN''.new; my $s = 0;|for ^20000 { $s += @objs[0].m }'
    'sub call vs frame locals (control)|500|1|sub f($x) { $x + 1 }; LOCALS my $s = 0;|for ^20000 { $s = f($s) }'
    '@a[5]++ vs array size (control)|100000|1|my @a = ^NN;|for ^20000 { @a[5]++ }'
    # --- #9173: the minor findings of the VM opcode audit -------------------
    'temp @a vs nested element size|2000|1|my @a = (^100).map({ [^NN] });|for ^200 { temp @a }'
    # #9434: a multi-level element temp saves the one element, not the base.
    'temp $t[1]<k>[1] vs container size|20000|1|my $t = [[^NN], { k => [0, 0] }];|for ^2000 { temp $t[1]<k>[1] = 5 }'
    'temp @a[1] vs array size|20000|1|my @a = ^NN;|for ^2000 { temp @a[1] = 5 }'
    'goto vs code size|500|1|LOCALS my $gi = 0;|GL: $gi++; goto GL if $gi < 20000'
    # Run this one with MUTSU_JIT=off: with the JIT on, the one-time cranelift
    # compile of the enlarged chunk lands inside the timed body.
    'ResetStateLocals vs loop body size|500|1|my $never = 0; my $s = 0;|for ^20000 { for ^1 { state $x = 1; $s += $x; if $never { STMTS } } }'
    'map callback vs frame locals|1000|1|LOCALS my $s = 0;|for ^20 { $s += (1..2000).map({ $_ + 1 }).elems }'
    'declaring a chain of NN classes|200|4|use MONKEY-SEE-NO-EVAL; my $src = "class D0 \{ \}; " ~ (1..NN).map({ "class D$_ is D{$_ - 1} \{ \}; " }).join;|EVAL $src'
)

gen_locals() {
    local n=$1 i out=""
    for ((i = 0; i < n; i++)); do out+="my \$lv$i = $i; "; done
    printf '%s' "$out"
}

gen_stmts() {
    local n=$1 i out=""
    for ((i = 0; i < n; i++)); do out+="\$s++; "; done
    printf '%s' "$out"
}

gen_classes() {
    local n=$1 i out="class C0 { method m { 1 } }; "
    for ((i = 1; i <= n; i++)); do out+="class C$i is C$((i - 1)) { }; "; done
    printf '%s' "$out"
}

time_case() {
    local setup=$1 body=$2 n=$3 f="$TMPDIR_CX/case.raku"
    setup=${setup//LOCALS/$(gen_locals "$n")}
    setup=${setup//CLASSES/$(gen_classes "$n")}
    setup=${setup//NN/$n}
    body=${body//STMTS/$(gen_stmts "$n")}
    body=${body//NN/$n}
    printf '%s\nmy $cx-t0 = now; %s; note "CXTIME ", now - $cx-t0;\n' "$setup" "$body" >"$f"
    timeout 300 "$BIN" "$f" 2>&1 >/dev/null | awk '/^CXTIME /{print $2}' | tail -1
}

printf '%-40s %7s %10s %10s %6s %s\n' case N 't(N)' 't(2N)' expect ratio
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
    printf '%-40s %7d %10.4f %10.4f %6s %s\n' "$name" "$n" "${t1:-0}" "${t2:-0}" "$expect" "$verdict"
done
