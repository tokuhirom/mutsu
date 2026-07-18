use v6;
use Test;

plan 2;

# A bare `Mu`/`Any` constraint is no narrower than an unconstrained param;
# it must not make a candidate outrank one with a genuinely narrow param
# (JSON::Unmarshal's `($json, @x)` vs its `(Any:D $json, Mu)` fallback).
class Dog {}

multi f($j, @x) { "pos" }
multi f(Any:D $j, Mu) { "mu" }

my @a = (1,);
is f(@a, Positional[Dog]), 'pos', 'the @-param candidate wins for a Positional arg';
is f(1, 2), 'mu', 'non-Positional second arg reaches the fallback';

