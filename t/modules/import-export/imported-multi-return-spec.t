use v6;
use lib 't/fixtures/imported-multi-return-spec/lib';
use Test;
use MultiReturnSpec;

plan 6;

# The return spec of an imported multi candidate is its own: a definite
# value (`--> 5`, `--> True`) is what the call answers, a type is enforced.
is definite("x"), 5, 'definite integer return spec of an imported multi';
is definite(3), 6, 'each candidate keeps its own';
is flag("abc", 1), True, 'definite True return spec (P5seek shape)';
is via-proto("x"), 9, 'candidate behind an exported proto';
throws-like { typed("x") }, X::TypeCheck::Return, 'a return type is enforced';

multi sub local-definite(Str $s --> 5) { 7 }
is &local-definite.candidates[0]("x"), 5, 'a candidate code object answers its definite spec';
