use v6;
use Test;

# `unit role Foo is export;` and `unit module Foo is export;` — lowercase
# bareword traits (`is export`, `is rw`, custom `is Foo(...)`) and `does Role`
# composition on a unit-scoped role/module declaration must be parsed, NOT left
# dangling to trigger "Confused. expected statement". Regression: mutsu's
# `unit role`/`unit module` branches consumed only `:adverb<...>` colon-traits,
# so `is export` after the name was an unparsed leftover that aborted the whole
# compilation unit.
#
# EVAL of a `unit ...;` compilation unit returns the declared type object.

plan 6;

# `is export` on a unit role parses; the result is the role type object.
{
    my $r = EVAL 'unit role UR0 is export; method who() { "UR0" }';
    is $r.^name, 'UR0', 'unit role is export parses and yields the role type';
}

# `is rw` on a unit role stays a trait, not a parent.
{
    my $r = EVAL 'unit role UR1 is rw; method tag() { "rw-role" }';
    is $r.^name, 'UR1', 'unit role is rw stays a trait';
}

# `does OtherRole` composition on a unit role parses and actually composes:
# the composing role inherits the parent role's method.
{
    role Base2 { method base() { "base" } }
    my $r = EVAL 'unit role UR2 does Base2; method own() { "own" }';
    my $o = Mu.new but $r;
    is $o.base, 'base', 'unit role does Role composes the parent role';
    is $o.own,  'own',  'unit role does Role keeps its own methods';
}

# `is export` on a unit module parses (no dangling-statement error). The
# regression was a parse failure, so `lives-ok` on EVAL is the pin.
{
    lives-ok { EVAL 'unit module UM0 is export; our sub answer() { 42 }' },
        'unit module is export parses without a dangling-statement error';
}

# A custom parenthesized trait on a unit module is consumed too.
{
    lives-ok { EVAL 'unit module UM1 is export(:MANDATORY); my $x = 7' },
        'unit module is export(:tag) consumes the parenthesized argument';
}
