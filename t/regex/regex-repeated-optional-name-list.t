use Test;

plan 6;

# A capture name that can bind twice in one match is list-valued even when
# every occurrence is optional and none of them bound (Rakudo's static
# capture-name analysis).
grammar G {
    token TOP { <a>? ':' <a>? }
    token a { x }
}
my $m = G.parse(':');
ok $m<a> ~~ Array, 'two optional occurrences that bound nothing give an Array';
is $m<a>.elems, 0, '... an empty one';

$m = G.parse('x:');
ok $m<a> ~~ Array, 'one of two optional occurrences bound: still an Array';
is $m<a>.elems, 1, '... holding the one Match';

grammar H {
    token TOP { <a>? ':' <a> }
    token a { x }
}
is H.parse(':x')<a>.elems, 1, 'optional plus required occurrence is a one-element Array';

grammar I {
    token TOP { <a>? b }
    token a { x }
}
nok I.parse('b')<a>.defined, 'a single optional occurrence stays Nil';
