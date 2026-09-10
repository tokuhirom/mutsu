use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# RakuAST Phase 5 slice 15 (ADR-0011): EVAL of the ternary `COND ?? THEN !! ELSE`.
# `RakuAST::Ternary` lowers to `Expr::Ternary`. Verified against Rakudo; passes
# under BOTH mutsu and raku.

plan 5;

# --- a plain ternary picks the branch by the condition ----------------------
is EVAL(Q[1 ?? 10 !! 20].AST), 10, 'ternary: true condition picks THEN';
is EVAL(Q[0 ?? 10 !! 20].AST), 20, 'ternary: false condition picks ELSE';

# --- a ternary returning booleans -------------------------------------------
is EVAL(Q[sub even($n) { $n %% 2 ?? True !! False }; even(4)].AST), True,
    'ternary yields True for an even number';
is EVAL(Q[sub even($n) { $n %% 2 ?? True !! False }; even(5)].AST), False,
    'ternary yields False for an odd number';

# --- a nested ternary (right-associative) -----------------------------------
is EVAL(Q[sub sgn($n) { $n > 0 ?? 1 !! $n < 0 ?? -1 !! 0 }; sgn(-5)].AST), -1,
    'nested ternary: sgn(-5) == -1';
