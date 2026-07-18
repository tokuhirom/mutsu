use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# RakuAST Phase 5 slice 12 (ADR-0011): EVAL of sub parameters with defaults.
# A `Parameter` carrying a `default` expression lowers to an optional positional
# `ParamDef` whose default is used when the argument is omitted. Verified against
# Rakudo; passes under BOTH mutsu and raku.

plan 4;

# --- default is used when the argument is omitted ---------------------------
is EVAL(Q[sub f($x, $y = 10) { $x + $y }; f(5)].AST), 15,
    'default parameter value is used when the argument is omitted';

# --- default is overridden by an explicit argument --------------------------
is EVAL(Q[sub f($x, $y = 10) { $x + $y }; f(5, 20)].AST), 25,
    'an explicit argument overrides the default';

# --- a default expression may reference an earlier parameter ----------------
is EVAL(Q[sub f($x, $y = $x * 2) { $x + $y }; f(5)].AST), 15,
    'a default expression can reference an earlier parameter';

# --- a plain (non-defaulted) multi-parameter sub still works ----------------
is EVAL(Q[sub add($a, $b) { $a + $b }; add(3, 4)].AST), 7,
    'a plain multi-parameter sub still lowers correctly';
