use v6;
use Test;

# An allomorph binds to a parameter typed as any of its component types,
# exactly like `~~` matches it. Regression: `sub foo(Int $x){}; foo <42>`
# threw "Type check failed in binding $x: expected Int, got IntStr" because
# the fast parameter type-check matched only the plain scalar variant and
# never unwrapped the allomorph Mixin.

plan 11;

# IntStr satisfies Int, Str, Cool, Numeric, Any
lives-ok { sub f(Int  $x) {}; f <42> }, 'IntStr binds Int';
lives-ok { sub f(Str  $x) {}; f <42> }, 'IntStr binds Str';
lives-ok { sub f(Cool $x) {}; f <42> }, 'IntStr binds Cool';
lives-ok { sub f(Any  $x) {}; f <42> }, 'IntStr binds Any';

# NumStr satisfies Num; RatStr satisfies Rat
lives-ok { sub f(Num $x) {}; f <42e0> }, 'NumStr binds Num';
lives-ok { sub f(Rat $x) {}; f <1/2> },  'RatStr binds Rat';

# the bound value keeps its allomorphic identity
sub name-of(Int $x) { $x.^name }
is name-of(<42>), 'IntStr', 'bound value is still an IntStr';

# `but`-mixed value still binds its base type
lives-ok { sub f(Int $x) {}; f (42 but True) }, 'mixed Int binds Int';

# negatives: a plain value must still be rejected against the wrong type
dies-ok { sub f(Str $x) {}; f 42 },  'plain Int does not bind Str';
dies-ok { sub f(Int $x) {}; f "x" }, 'plain Str does not bind Int';
dies-ok { sub f(Int $x) {}; f <4e0> }, 'NumStr does not bind Int';
