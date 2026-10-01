use v6;
use Test;

# `GLOBAL::<$g> = v` writes the variable; `Pkg::<@a> = v` / `Pkg::<%h> = v`
# is an assignment to an immutable value (#10425). Measured against rakudo.

plan 8;

our $g = 1;
GLOBAL::<$g> = 7;
is $g, 7, 'GLOBAL::<$g> = v stores into the variable';
is $GLOBAL::g, 7, '... visible through $GLOBAL::g';

GLOBAL::<$fresh> = 'new';
is GLOBAL::<$fresh>, 'new', 'a never-declared GLOBAL::<$x> reads back';
is $GLOBAL::fresh, 'new', '... through $GLOBAL::x too';

package Q { our @a; our %h }

throws-like { Q::<@a> = (1, 2) }, X::AdHoc,
    message => /'Cannot assign to an immutable value'/,
    'Pkg::<@a> = v dies';
throws-like { Q::<%h> = (a => 1) }, X::AdHoc,
    message => /'Cannot assign to an immutable value'/,
    'Pkg::<%h> = v dies';
is @Q::a.elems, 0, '... and leaves the array untouched';
is %Q::h.elems, 0, '... and the hash untouched';
