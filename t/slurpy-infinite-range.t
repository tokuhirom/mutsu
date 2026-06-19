use Test;

plan 5;

# Binding an infinite range (`1..*`) to a flattening slurpy must not hang:
# the range is expanded to a bounded prefix instead of looping to i64::MAX.
# (A truly lazy slurpy is the goal of docs/lazy-arrays.md Slice L4; this just
# guarantees termination.)

sub star(*@x) { @x[^3] }
is-deeply star(1..*), (1, 2, 3), '*@ slurpy bound an infinite range does not hang';

sub plus(+@x) { @x[^3] }
is-deeply plus(1..*), (1, 2, 3), '+@ slurpy bound an infinite range does not hang';

# A finite range still flattens fully.
sub all(*@x) { @x.elems }
is all(1..5), 5, 'finite range flattens fully';

# Mixed: finite args plus an infinite range.
sub mixed(*@x) { @x[^4] }
is-deeply mixed(0, 1..*), (0, 1, 2, 3), 'finite arg followed by an infinite range does not hang';

# A normal small range is unaffected and exact.
is-deeply all(1..3).item, 3, 'small finite range count unchanged';
