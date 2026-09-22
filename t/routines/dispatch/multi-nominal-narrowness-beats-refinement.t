use v6;
use Test;

# Rakudo ranks multi candidates in two tiers: it compares their NOMINAL
# parameter types first, and only consults a parameter's refinement -- a
# `where` clause, or the equality check a literal or a `subset` compiles to --
# when the two candidates are tied on every nominal type. mutsu's rank tuple
# used to compare `where_count` BEFORE the typed-parameter count, so merely
# HAVING a `where` clause made a candidate narrower than a plainly-typed
# sibling whose nominal type was actually narrower
# (https://github.com/tokuhirom/mutsu/issues/8958).
#
# Every expectation below was verified against rakudo.

plan 14;

# --- the reported repro ------------------------------------------------------
# `42` does not compose R2, so the untyped where-clause candidate must be
# passed over for the plainly-typed `Int $x` one.

{
    role R2 { }
    multi sub f($x where * does R2) { 'does-R2' }
    multi sub f(Int $x)             { 'is-Int' }
    is f(42), 'is-Int', 'an untyped where-clause candidate loses to a typed one';
}

# --- a `where` still breaks a tie between equally-typed candidates ------------

{
    multi sub g(Int $x where * > 5) { 'Int-where' }
    multi sub g(Int $x)             { 'Int' }
    is g(42), 'Int-where', 'a where clause still wins between same-typed candidates';
}

{
    multi sub h($x where * > 5) { 'where' }
    multi sub h($x)             { 'plain' }
    is h(42), 'where', 'a where clause still wins between two untyped candidates';
}

# --- type-hierarchy distance outranks the refinement too ---------------------
# Both candidates carry one meaningfully-typed positional, so the nominal
# counts tie; `Int` is nearer to `42` than `Cool` is, and that settles it
# before the `where` clause is weighed at all.

{
    multi sub i(Cool $x where * > 5) { 'Cool-where' }
    multi sub i(Int $x)              { 'Int' }
    is i(42), 'Int', 'a nearer nominal type beats a wider one carrying a where';
}

{
    multi sub j($x where * > 5) { 'where' }
    multi sub j(Cool $x)        { 'Cool' }
    is j(42), 'Cool', 'any declared type beats an untyped where-clause candidate';
}

# --- refinements that are NOT plain `where` clauses keep their standing -------

{
    multi sub k(42)       { 'literal' }
    multi sub k(Int $x)   { 'Int' }
    is k(42), 'literal', 'a literal parameter still beats its own nominal type';
}

{
    multi sub l(42)      { 'literal' }
    multi sub l(Cool $x) { 'Cool' }
    is l(42), 'literal', 'a literal parameter still beats a wider nominal type';
}

{
    subset Even of Int where * %% 2;
    multi sub m(Even $x) { 'Even' }
    multi sub m(Int $x)  { 'Int' }
    is m(42), 'Even', 'a subset still beats the base type it refines';
}

{
    multi sub n(UInt $x) { 'UInt' }
    multi sub n(Int $x)  { 'Int' }
    is n(42), 'UInt', 'the core UInt subset still beats Int';
}

{
    subset Mid of Int where * > 0;
    subset Top of Mid where * < 100;
    multi sub o(Top $x) { 'Top' }
    multi sub o(Int $x) { 'Int' }
    is o(42), 'Top', 'a subset of a subset resolves to the same nominal base';
}

{
    subset Even2 of Int where * %% 2;
    multi sub p(Even2 $x)        { 'Even2' }
    multi sub p($x where * > 5)  { 'where' }
    is p(42), 'Even2', 'a subset beats an untyped where-clause candidate';
}

{
    multi sub q([$a, $b])        { 'subsig' }
    multi sub q(Positional $x)   { 'Positional' }
    is q([1, 2]), 'subsig', 'a sub-signature still beats the bare Positional type';
}

# --- the tiers are compared across ALL positionals, not one at a time ---------

{
    multi sub r($a where * > 5, Str $b) { 'where-Str' }
    multi sub r(Int $a, Str $b)         { 'Int-Str' }
    is r(42, 'x'), 'Int-Str', 'the nominal count is summed over every positional';
}

# --- a surplus optional positional does not make a candidate wider -----------
# The two candidates share a minimum arity of 0, so rakudo compares only the
# first positional and lets the guard's `where` break the resulting tie.

{
    multi sub s(Int $seed = 0)                      { 'short' }
    multi sub s(Int $seed = 0, $? where { True })   { 'guarded' }
    is s(7), 'guarded', 'a trailing where-guard on an unsupplied optional still wins';
}

done-testing;
