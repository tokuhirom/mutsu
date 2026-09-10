use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# RakuAST slice 22 (ADR-0011): named parameters `:$x`, both directions. A named
# `Parameter` carries a `names` list (and no `optional`, since named params are
# optional by default); the write side sets the `ParamDef`'s `named` flag. The
# `x => 5` call argument (a `FatArrow` infix) also lowers now. Verified against
# Rakudo; passes under BOTH mutsu and raku.

plan 5;

# --- read side: a named parameter renders with a `names` list ---------------
ok Q[sub f(:$x) { $x }].AST.gist.contains('names  => ('),
    'a named parameter renders with a names list';

# --- a named argument binds the parameter -----------------------------------
is EVAL(Q[sub f(:$x) { $x * 2 }; f(x => 5)].AST), 10,
    'a `key => value` argument binds a named parameter';

# --- the colonpair argument form --------------------------------------------
is EVAL(Q[sub f(:$x) { $x * 2 }; f(:x(7))].AST), 14,
    'a `:x(value)` argument binds a named parameter';

# --- a positional and a named parameter together ----------------------------
is EVAL(Q[sub g($a, :$b) { $a + $b }; g(3, b => 4)].AST), 7,
    'a positional and a named parameter combine';

# --- an omitted named parameter is undefined --------------------------------
is EVAL(Q[sub f(:$x) { $x.defined }; f()].AST), False,
    'an omitted named parameter is undefined';
