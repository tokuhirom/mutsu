use v6;
use Test;

# Found via the fez distribution (Fez::Util::Glob):
# `%spesh{$c}:exists??'\\'!!''` -- the adverb is immediately followed by `??`.
plan 6;

my %h = a => 1;
my @a = 5, 6;

is %h<a>:exists??'Y'!!'N', 'Y', ':exists directly followed by ??';
is %h<b>:exists??'Y'!!'N', 'N', ':exists (missing key) directly followed by ??';
is @a[1]:exists??'Y'!!'N', 'Y', 'positional :exists directly followed by ??';
is "{%h{'a'}:exists??'\\'!!''}x", '\\x', 'inside a string interpolation block';
is %h<a>:exists ?? 'Y' !! 'N', 'Y', 'spaced form still works';
is (%h<a>:!exists)??'Y'!!'N', 'N', ':!exists form';
