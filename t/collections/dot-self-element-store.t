use Test;

# `.self` on a plain Array/Hash hands back the receiver's own container, so an
# element store through it writes in place (#9197).

plan 8;

my $p = [1, 2];
$p.self[0] = 5;
is-deeply $p, [5, 2], 'scalar-held Array: $p.self[0] = 5';

my @b = 1, 2;
my $r = @b;
$r.self[1] = 7;
is-deeply @b, [1, 7], 'store through .self reaches the aliased @-variable';

$p.self[3] = 9;
is $p.elems, 4, 'store past the end through .self grows the Array';

my $h = {a => 1};
$h.self<k> = 5;
is-deeply $h, {a => 1, k => 5}, 'scalar-held Hash: $h.self<k> = 5';

my %hh = a => 1;
my $rh = %hh;
$rh.self<z> = 9;
is %hh<z>, 9, 'store through .self reaches the aliased %-variable';

throws-like { (1, 2).self[0] = 5 }, X::Assignment::RO,
    message => 'Cannot modify an immutable List ((1 2))',
    'a List literal stays immutable through .self';

my $l = (1, 2);
throws-like { $l.self[0] = 5 }, X::Assignment::RO,
    'a scalar-held List stays immutable through .self';
is-deeply $l, (1, 2), 'the refused store does not leak into the variable';
