use Test;

# A slurpy candidate competes with an exact-arity candidate that takes an
# optional positional (#12791). Rakudo compares the two on their shared
# positions when their minimum arities agree, and prefers the non-slurpy one
# outright only when both the positional count and the minimum arity differ.
# Expected values from rakudo.

plan 9;

multi sub a(Int $x, *@r)      { "slurpy" }
multi sub a(Any $x, Any $y?)  { "two-opt" }
is a(1, 2), 'slurpy', 'equal minimum arity: the narrower shared type wins over an optional';

multi sub b(Any $x, Any $y?)  { "two-opt" }
multi sub b(Int $x, *@r)      { "slurpy" }
is b(1, 2), 'slurpy', 'whatever the declaration order';

multi sub c(Int $x, *@r)      { "slurpy" }
multi sub c(Any $x, Any $y)   { "two" }
is c(1, 2), 'two', 'both arities differ: the non-slurpy candidate wins';

multi sub d(Int $x, *@r)               { "slurpy" }
multi sub d(Any $x, Any $y, Any $z?)   { "opt3" }
is d(1, 2), 'opt3', 'both arities differ with an optional: the non-slurpy candidate wins';

multi sub e(*@r)              { "slurpy" }
multi sub e(Int $x?)          { "opt" }
is e(1), 'opt', 'an optional typed positional beats a bare slurpy';

proto p(|) {*}
multi p(Int $x, *@r)          { "slurpy" }
multi p(Any $x, Any $y?)      { "two-opt" }
is p(1, 2), 'slurpy', 'through a proto';

module M {
    our proto s(|) {*}
    multi s(Int $x, *@r)      { "slurpy" }
    multi s(Any $x, Any $y?)  { "two-opt" }
}
is M::s(1, 2), 'slurpy', 'through a package-qualified call';

multi sub f(Int $x, *@r)      { "slurpy " ~ callsame() }
multi sub f(Any $x, Any $y?)  { "two-opt" }
is f(1, 2), 'slurpy two-opt', 'callsame reaches the optional candidate next';

multi sub g(Int $x, Int $y?)  { "opt" }
multi sub g(Int $x)           { "one" }
is g(1), 'one', 'an exact candidate without the optional still wins';
