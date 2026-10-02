use v6;
use Test;

# From Deps: `for Type.^roles -> ::Type { ... }` -- a type capture as the
# pointy parameter of a `for` loop.
plan 7;

for 1 -> ::T { is T.^name, 'Int', 'bare ::T' }
for <a b> -> ::T { is T.^name, 'Str', 'bare ::T, second loop' }
for 1 -> ::T $x { is T.^name ~ $x, 'Int1', '::T $x' }
for 1.5, 2.5 -> $a, ::T { is T.^name, 'Rat', 'second param of a multi-param pointy' }
my @seen;
for ^2 -> ::T { @seen.push: T.^name }
is-deeply @seen, ['Int', 'Int'], 'rebound per iteration';
my &f = -> ::T { T.^name };
is f(1), 'Int', 'lambda form still works';
