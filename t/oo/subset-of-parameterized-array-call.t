use v6;
use Test;

plan 4;

# Calling a subset whose refinee is a parameterized type coerces through the
# refinee's coercion protocol (`Array[Name](value)`), not a method named
# `Array[Name]`.
subset Name of Str;
subset Names of Array[Name];

my $r = Names(<a b>.Seq);
is $r.raku, 'Array[Name].new("a", "b")', 'a Seq becomes the typed array';
isa-ok $r, Array, 'it is an Array';
ok $r ~~ Names, 'and satisfies the subset';

subset Short of Int where * < 100;
is Short("50"), 50, 'a plain refinee still coerces by its method';
