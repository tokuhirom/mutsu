use v6.e.PREVIEW;
use Test;

plan 7;

my @a;
my @i := (0, 1), 0;

is-deeply (@a[|| @i] = 7), (7, Any),
    'a scalar RHS returns every slot selected by a dimension splat';
is-deeply @a.raku, '[[7], [Any]]',
    'the scalar is stored in the first selected slot and the next slot stays Any';

my @b;
is-deeply (@b[|| @i] = 42, 666), (42, 666),
    'a list RHS keeps the assigned-slot result';

my @c;
is (@c[0; 0] = 9), 9,
    'a scalar-only multi-dimensional assignment still returns its scalar RHS';

my @d;
is-deeply (@d[0, 1] = 1, 2, 3), (1, 2),
    'a slice result contains selected slots and omits excess RHS values';

my %h;
%h<a> = [];
is-deeply (%h<a>[|| @i] = 12), (12, Any),
    'a slice assignment through a subscript chain returns all assigned slots';
is-deeply %h<a>.raku, '$[[12], [Any]]',
    'the chained slice assignment writes each selected slot';
