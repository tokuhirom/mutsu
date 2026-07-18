use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# RakuAST Phase 5 slice 5 (ADR-0011): EVAL of `if`/`while` and `$x = EXPR`
# assignment. `Statement::If` lowers to a conditional, `Statement::Loop::While`
# to a while-loop, and an `ApplyInfix` whose infix is an `Assignment` lowers to
# an assignment statement. Verified against Rakudo; passes under BOTH mutsu and
# raku.

plan 6;

# --- if / else --------------------------------------------------------------
is EVAL(Q[my $x = 5; if $x > 3 { 10 } else { 20 }].AST), 10,
    'if: true branch is taken';
is EVAL(Q[my $x = 1; if $x > 3 { 10 } else { 20 }].AST), 20,
    'if: else branch is taken';

# an if with no else and a false condition yields an undefined value
nok EVAL(Q[my $x = 1; if $x > 3 { 10 }].AST).defined,
    'if without else: false condition is undefined';

# --- assignment mutates a lexical -------------------------------------------
is EVAL(Q[my $s = 1; $s = $s + 4; $s].AST), 5,
    'assignment mutates a lexical and the new value is visible';

# --- while loop -------------------------------------------------------------
is EVAL(Q[my $i = 0; my $s = 0; while $i < 3 { $s = $s + $i; $i = $i + 1 }; $s].AST), 3,
    'while: accumulate 0 + 1 + 2 == 3';
is EVAL(Q[my $n = 5; my $f = 1; while $n > 1 { $f = $f * $n; $n = $n - 1 }; $f].AST), 120,
    'while: 5! == 120';
