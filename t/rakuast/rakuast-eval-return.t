use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# RakuAST Phase 5 slice 10 (ADR-0011): EVAL of `return`/`last`/`next`.
# raku models these control-flow statements as bare calls (`Call::Name` in
# WithoutParentheses form); the read side (`.AST`) emits that form and the write
# side (`EVAL`) lowers it back to the internal control-flow statement. Verified
# against Rakudo; passes under BOTH mutsu and raku.

plan 6;

# --- read side: the gist round-trips through raku's call form ---------------
ok Q[sub f { return 5 }].AST.gist.contains('RakuAST::Call::Name::WithoutParentheses.new('),
    'return renders as a WithoutParentheses call';
ok Q[sub f { return 5 }].AST.gist.contains('RakuAST::Name.from-identifier("return")'),
    'the call name is "return"';

# --- early return -----------------------------------------------------------
is EVAL(Q[sub f($x) { if $x > 0 { return 5 }; -1 }; f(3)].AST), 5,
    'early return: taken branch returns 5';
is EVAL(Q[sub f($x) { if $x > 0 { return 5 }; -1 }; f(-3)].AST), -1,
    'early return: fall-through returns -1';

# --- last stops the loop ----------------------------------------------------
is EVAL(Q[my $s = 0; for 1..10 -> $x { last if $x > 3; $s = $s + $x }; $s].AST), 6,
    'last: sum 1+2+3 before breaking == 6';

# --- next skips an iteration ------------------------------------------------
is EVAL(Q[my $s = 0; for 1..5 -> $x { next if $x %% 2; $s = $s + $x }; $s].AST), 9,
    'next: sum of odds 1+3+5 == 9';
