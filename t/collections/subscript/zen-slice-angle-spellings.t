use v6;
use Test;

# Empty angle subscripts are zen slices in all three spellings. The
# interpolating guillemet and ASCII double-angle forms must not turn an empty
# key list into an empty result.

plan 16;

my @a = 10, 20;
is @a<>.elems, 2, 'plain angle zen slice keeps the array';
is @a«».elems, 2, 'guillemet zen slice keeps the array';
is @a<<>>.elems, 2, 'double-angle zen slice keeps the array';
is @a.<>.elems, 2, 'dotted plain angle zen slice keeps the array';
is @a.«».elems, 2, 'dotted guillemet zen slice keeps the array';
is @a.<<>>.elems, 2, 'dotted double-angle zen slice keeps the array';

my %h = a => 1, b => 2;
is-deeply %h<>:k.sort.List, ('a', 'b'), 'plain angle zen slice maps :k';
is-deeply %h«»:k.sort.List, ('a', 'b'), 'guillemet zen slice maps :k';
is-deeply %h<<>>:k.sort.List, ('a', 'b'), 'double-angle zen slice maps :k';
is-deeply %h.<>:v.sort.List, (1, 2), 'dotted plain angle zen slice maps :v';
is-deeply %h.«»:v.sort.List, (1, 2), 'dotted guillemet zen slice maps :v';
is-deeply %h.<<>>:v.sort.List, (1, 2), 'dotted double-angle zen slice maps :v';

is %h«»:kv.elems, 4, 'guillemet zen slice maps :kv';
is %h<<>>:p.elems, 2, 'double-angle zen slice maps :p';
is %h.«»:kv.elems, 4, 'dotted guillemet zen slice maps :kv';
is %h.<<>>:p.elems, 2, 'dotted double-angle zen slice maps :p';

done-testing;
