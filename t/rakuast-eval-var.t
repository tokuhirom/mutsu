use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# RakuAST Phase 5 slice 3 (ADR-0011): EVAL of variable declarations and uses.
# `my $x = EXPR` lowers to Stmt::VarDecl and `$x` to a variable expression, so a
# multi-statement program round-trips through .AST and runs. Verified against
# Rakudo; this file passes under BOTH mutsu and raku.

plan 4;

is EVAL(Q[my $x = 5; $x * 2].AST), 10, 'declare, then use: $x * 2 == 10';
is EVAL(Q[my $a = 3; my $b = 4; $a + $b].AST), 7, 'two declarations sum to 7';
is EVAL(Q[my $n = 10; $n - 1].AST), 9, 'declare 10, then $n - 1 == 9';

# --- a bare Var::Lexical lowers on its own ----------------------------------
is EVAL(Q[my $x = 42; $x].AST), 42, 'a variable use round-trips to its value';
