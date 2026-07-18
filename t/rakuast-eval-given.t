use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# RakuAST Phase 5 slice 8 (ADR-0011): EVAL of `given`/`when`/`default`.
# `Statement::Given`/`When`/`Default` lower to the topicalizer and its match
# clauses, so a `given` block runs and yields the matched clause's value.
# Verified against Rakudo; passes under BOTH mutsu and raku.

plan 5;

# --- given as the tail value: a matching `when` yields its value ------------
is EVAL(Q[my $x = 2; given $x { when 1 { 10 }; when 2 { 20 }; default { 30 } }].AST), 20,
    'given/when: the matching clause value is the given value';
is EVAL(Q[my $x = 9; given $x { when 1 { 10 }; when 2 { 20 }; default { 30 } }].AST), 30,
    'given/default: the default value when no when matches';

# --- given as a non-tail statement: its value is sunk, the tail wins --------
is EVAL(Q[given 2 { when 2 { 99 } }; 7].AST), 7,
    'a non-tail given is sunk; the following statement is the value';

# --- a `when` body with a side effect, read after the given -----------------
is EVAL(Q[my $out = 0; given 2 { when 1 { $out = 100 }; when 2 { $out = 200 }; default { $out = 300 } }; $out].AST), 200,
    'a when clause mutates a lexical visible after the given';

# --- given inside a sub -----------------------------------------------------
is EVAL(Q[sub classify($n) { given $n { when 0 { "zero" }; default { "nonzero" } } }; classify(0)].AST), 'zero',
    'given inside a sub returns the matched clause value';
