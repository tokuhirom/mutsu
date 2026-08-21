use v6;
use lib 't/lib';
use Test;
use ListopMultiExtendsCore;

# ADR-0044 D1 section 2.2: an imported `multi` for a core listop name (with
# no accompanying `proto` export) ADDS a candidate to CORE's dispatch set --
# the core array form must stay reachable alongside it. Contrast with
# t/listop-shadow-imported.t, whose fixture exports its own `proto`, which
# fully replaces the dispatch set instead of extending it.

plan 3;

is &splice.defined, True,
    'an imported multi splice makes &splice a defined routine';
is splice("", 0, "Raku"), 'custom  0 Raku',
    'and the imported candidate is reachable';

my @a = (1, 2, 3, 4, 5);
splice(@a, 1, 2);
is-deeply @a, [1, 4, 5],
    'the core array splice form still works alongside the import';
