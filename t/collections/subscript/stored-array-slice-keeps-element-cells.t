use Test;

# An Array slice is a `List` of the Array's own element containers. A `$`
# variable assigned one keeps them, so writing an element through it later
# reaches the Array, while assigning the variable itself replaces it (#17401).
# Checked against rakudo.

plan 10;

my @e = 1, 2;
my $s = @e[0..1];
lives-ok { $s.map({ $_ = 8 }).sink }, 'a stored slice can be written through';
is-deeply @e, [8, 8], '... and the write reaches the Array';

my @b = 1, 2, 3;
my $t;
$t = @b[0, 2];
$t.map({ $_ *= 10 }).sink;
is-deeply @b, [10, 2, 30], 'a list-subscript slice assigned to a declared $ variable';

my @h = 1, 2;
my $v = @h[0..1];
$v[0] = 5;
is-deeply @h, [5, 2], 'an element assignment through the stored slice';

my @i = 1, 2;
my $w = @i[0..1];
@i[0] = 7;
is $w.gist, '(7 2)', 'the stored slice sees a later write to the Array';

my @a = 1, 2, 3;
my $r = @a[0..1];
$r = 5;
is-deeply (@a, $r), ([1, 2, 3], 5), 'assigning the variable replaces the slice';

my @c = 1, 2, 3;
my $u = @c[1..2];
is-deeply ($u.sum, $u.elems, $u eqv (2, 3)), (5, 2, True), 'the stored slice reads as its values';
is $u.WHAT.^name, 'List', '... and is a List';

my @d = 1, 2, 3;
my $x = @d[0..1];
my @copy = |$x;
@copy[0] = 99;
is-deeply @d, [1, 2, 3], 'an Array built from it is a copy';

my @f = 1, 2;
my $y = @f[0];
$y = 9;
is-deeply @f, [1, 2], 'a single element read is a value, not the element';
