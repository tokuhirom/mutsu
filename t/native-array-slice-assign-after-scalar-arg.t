use Test;

plan 8;

# Passing an array to a *scalar* parameter boxes the caller's binding into a
# container cell for writeback. Everything user-visible reads through that cell,
# so the array must keep behaving as the native typed array it is -- in
# particular a Range subscript on the left of `=` must still be a slice.
sub takes-scalar(Mu $x) { ?$x }

my @a := array[num].new;
@a[0] = 1e0;
takes-scalar(@a);
@a[^3] = 5e0, 6e0, 7e0;
is-deeply @a.List, (5e0, 6e0, 7e0), 'Range slice assign survives a scalar-param call';
is @a.of.^name, 'num', 'the array is still native';

my @b := array[int].new;
@b[0] = 1;
takes-scalar(@b);
@b[1..2] = 8, 9;
is-deeply @b.List, (1, 8, 9), 'an inclusive Range slice too';

my @c := array[str].new;
@c[0] = 'a';
takes-scalar(@c);
@c[^2] = 'x', 'y';
is-deeply @c.List, ('x', 'y'), 'and on a native str array';

# A comma-list subscript was never affected; keep it pinned alongside.
my @d := array[num].new;
takes-scalar(@d);
@d[0,1] = 3e0, 4e0;
is-deeply @d.List, (3e0, 4e0), 'a comma-list slice assign still works';

# Single-element assignment and reads are unaffected.
my @e := array[num].new;
takes-scalar(@e);
@e[2] = 2e0;
is @e.elems, 3, 'single-element assign still grows the array';
is-deeply @e[^3].List, (0e0, 0e0, 2e0), 'slice READ was always fine';

# Without the intervening call the slice assign has always worked.
my @f := array[num].new;
@f[^3] = 1e0, 2e0, 3e0;
is-deeply @f.List, (1e0, 2e0, 3e0), 'and with no call at all';
