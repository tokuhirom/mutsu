use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# RakuAST Phase 5 slice 7 (ADR-0011): EVAL of `sub` declarations and named calls.
# `RakuAST::Sub` (with bare positional scalar parameters) lowers to a sub
# declaration and a `Call::Name` lowers to a named call, so a defined routine can
# be invoked — including recursively. Verified against Rakudo; passes under BOTH
# mutsu and raku.

plan 5;

# --- a one-parameter sub, then call it --------------------------------------
is EVAL(Q[sub dbl($x) { $x * 2 }; dbl(21)].AST), 42,
    'sub with one parameter is declared and called';

# --- two parameters ---------------------------------------------------------
is EVAL(Q[sub add($a, $b) { $a + $b }; add(3, 4)].AST), 7,
    'sub with two parameters';

# --- a multi-statement body -------------------------------------------------
is EVAL(Q[sub poly($x) { my $s = $x * $x; $s + $x }; poly(4)].AST), 20,
    'sub with a multi-statement body';

# --- recursion (sub + if/else + call all compose) ---------------------------
is EVAL(Q[sub fact($n) { if $n < 2 { 1 } else { $n * fact($n - 1) } }; fact(5)].AST), 120,
    'recursive sub: 5! == 120';

# --- a call as an argument to another call ----------------------------------
is EVAL(Q[sub inc($n) { $n + 1 }; sub dbl($n) { $n * 2 }; dbl(inc(9))].AST), 20,
    'a call nested as an argument';
