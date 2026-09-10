use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# RakuAST Phase 5 slice 6 (ADR-0011): EVAL of `for` loops and `elsif` chains.
# `Statement::For` (with a single-parameter pointy block) lowers to a for-loop,
# and each `Statement::Elsif` in an `if`'s `elsifs` list folds into a nested
# conditional. Verified against Rakudo; passes under BOTH mutsu and raku.

plan 6;

# --- elsif chain (each branch reached via a fresh EVAL) ----------------------
is EVAL(Q[my $x = 5; if $x > 3 { 1 } elsif $x > 1 { 2 } else { 3 }].AST), 1,
    'elsif: first (if) branch';
is EVAL(Q[my $x = 2; if $x > 3 { 1 } elsif $x > 1 { 2 } else { 3 }].AST), 2,
    'elsif: middle (elsif) branch';
is EVAL(Q[my $x = 0; if $x > 3 { 1 } elsif $x > 1 { 2 } else { 3 }].AST), 3,
    'elsif: final (else) branch';

# two elsif clauses fold in source order
is EVAL(Q[my $x = 2; if $x > 5 { 10 } elsif $x > 3 { 20 } elsif $x > 1 { 30 } else { 40 }].AST), 30,
    'elsif: second of two elsif clauses';

# --- for loop with a pointy parameter ---------------------------------------
is EVAL(Q[my $s = 0; for 1..4 -> $x { $s = $s + $x }; $s].AST), 10,
    'for: sum 1..4 == 10';
is EVAL(Q[my $p = 1; for (2, 3, 4) -> $n { $p = $p * $n }; $p].AST), 24,
    'for: product of a list == 24';
