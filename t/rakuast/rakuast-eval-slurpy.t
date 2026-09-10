use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# RakuAST slice 23 (ADR-0011): slurpy parameters `*@a`, both directions. A slurpy
# `Parameter` carries a `slurpy => RakuAST::Parameter::Slurpy::Flattened` marker
# (a bare class-name node); the write side sets the `ParamDef`'s `slurpy` flag.
# Verified against Rakudo; passes under BOTH mutsu and raku.

plan 5;

# --- read side: the slurpy marker renders as a bare class name --------------
ok Q[sub f(*@a) { @a }].AST.gist.contains('slurpy => RakuAST::Parameter::Slurpy::Flattened'),
    'a slurpy parameter renders with the Flattened marker';

# --- a slurpy collects all positional arguments -----------------------------
is EVAL(Q[sub f(*@a) { @a.elems }; f(1, 2, 3)].AST), 3,
    'a slurpy collects every positional argument';

# --- a leading positional plus a slurpy -------------------------------------
is EVAL(Q[sub f($first, *@rest) { $first + @rest.elems }; f(10, 1, 2, 3)].AST), 13,
    'a leading positional plus a slurpy';

# --- the slurpy array supports list methods ---------------------------------
is EVAL(Q[sub total(*@n) { @n.sum }; total(1, 2, 3, 4)].AST), 10,
    'the slurpy array is a real list';

# --- an empty slurpy --------------------------------------------------------
is EVAL(Q[sub f(*@a) { @a.elems }; f()].AST), 0,
    'a slurpy with no arguments is empty';
