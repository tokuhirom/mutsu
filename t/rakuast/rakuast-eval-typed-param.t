use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# RakuAST slice 17 (ADR-0011): typed positional parameters `Int $x`, both
# directions. A `Parameter` carrying a `type => Type::Simple(Name)` lowers to a
# `ParamDef` with that type constraint (and the read side emits it). Verified
# against Rakudo; passes under BOTH mutsu and raku.

plan 5;

# --- read side: the gist has the Type::Simple constraint --------------------
my $g = Q[sub f(Int $x) { $x }].AST.gist;
ok $g.contains('RakuAST::Type::Simple.new(')
    && $g.contains('RakuAST::Name.from-identifier("Int")'),
    'a typed parameter renders with a Type::Simple';

# --- a typed parameter is accepted and used --------------------------------
is EVAL(Q[sub f(Int $x) { $x * 2 }; f(5)].AST), 10,
    'a typed Int parameter is bound and used';

# --- two typed parameters of different types --------------------------------
is EVAL(Q[sub g(Int $x, Str $s) { $s ~ $x }; g(3, "n=")].AST), 'n=3',
    'two typed parameters of different types';

# --- a typed parameter with a default ---------------------------------------
is EVAL(Q[sub h(Int $x, Int $y = 10) { $x + $y }; h(5)].AST), 15,
    'a typed parameter can also carry a default';

# --- a type mismatch is rejected --------------------------------------------
throws-like { EVAL(Q[sub f(Int $x) { $x }; f("hi")].AST) }, Exception,
    'passing the wrong type throws';
