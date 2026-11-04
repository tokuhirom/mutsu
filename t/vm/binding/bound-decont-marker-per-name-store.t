use Test;

# A scalar bound to a container (`my $b := @a`) is not a Scalar container: an
# `@c = $b` flattens it. The marker recording that is cleared by a later plain
# store to the name, and a store to a name that was never bound pays nothing for
# a bind made elsewhere in the program (#7817: the store asks the name's own
# bit, not only the program-wide latch).
plan 8;

my @a = 1, 2, 3;
my $b := @a;
my @c = $b;
is @c.elems, 3, 'a scalar bound to an array flattens on @-assignment';

# After a bind exists somewhere, plain scalars still store and read correctly.
my $s = 0;
for ^200 { $s = $s + $_ }
is $s, 19900, 'a plain scalar loop after a bind elsewhere';

sub itemized { my $q = (1, 2); my @z = $q; @z.elems }
is itemized(), 1, 'an assigned (not bound) scalar itemizes';

# The marker of a rebound name follows the latest binding.
my $r = 5;
my @t = $r;
is @t.elems, 1, 'an unbound scalar is one item';
{
    my $w := @a;
    my @u = $w;
    is @u.elems, 3, 'a block-local bind flattens';
}
{
    my $w = (7, 8);
    my @u = $w;
    is @u.elems, 1, 'a later declaration of the same name in a sibling block itemizes';
}

# A name stored in a loop and bound once, in turn.
my @sizes;
for 1..3 -> $i {
    my $m = $i;
    $m = $m + 1;
    my @n = $m;
    @sizes.push(@n.elems);
}
is @sizes.join(','), '1,1,1', 'stores to never-bound names in a loop';

my $late = 1;
$late = $late + 1;
my @l = $late;
is $late, 2, 'a store to a name beside a bound one';
