use Test;

# The `[notandthen]` reduce meta-operator. `notandthen` is the mirror of
# `andthen`: `$a notandthen $b` passes the RHS through when the LHS is
# *undefined*, otherwise it yields Empty. Reduced, the fold short-circuits to a
# (persistent) Empty as soon as a defined value is seen before the tail.

plan 13;

# The identity element of an empty reduction is True (like `[andthen]`).
is-deeply ([notandthen]), True, '[notandthen] () identity is True';

# A single element passes through unchanged.
is-deeply ([notandthen] 5), 5, '[notandthen] of one element is that element';

# Two elements: a defined LHS yields Empty; an undefined LHS passes the RHS.
is-deeply ([notandthen] 1, 2), Empty, '[notandthen] 1, 2 is Empty (1 defined)';
is-deeply ([notandthen] Nil, 2), 2, '[notandthen] Nil, 2 passes the RHS';

# Longer folds short-circuit at the first defined non-tail element.
is-deeply ([notandthen] 1, 2, True), Empty, 'first-element defined -> Empty';
is-deeply ([notandthen] Nil, Nil, 7), 7, 'all undefined until the tail -> tail';
is-deeply ([notandthen] Nil, 5, 7), Empty, 'a mid defined element -> Empty (persists)';

# The Empty is absorbing: a defined value mid-fold cannot be "undone" by a later
# undefined one.
is-deeply ([notandthen] Nil, 5, Nil, 7), Empty, 'Empty persists past a later Nil';

# Slip / list-flattening form (the doc `[notandthen] |@_, True` shape).
my @down = Nil, Nil, Nil;
is-deeply ([notandthen] |@down, True), True, 'all sensors down -> True';
my @up = Nil, 42, Nil;
is-deeply ([notandthen] |@up, True), Empty, 'a working sensor -> Empty';

# andthen's empty identity is likewise True (fixed alongside).
is-deeply ([andthen]), True, '[andthen] () identity is True';

# The doc-example sub built on the reduce.
sub all-sensors-down { [notandthen] |@_, True }
ok all-sensors-down(Nil, Nil, Nil), 'all-sensors-down is truthy when all Nil';
nok all-sensors-down(Nil, 42, Nil), 'all-sensors-down is false when one works';
