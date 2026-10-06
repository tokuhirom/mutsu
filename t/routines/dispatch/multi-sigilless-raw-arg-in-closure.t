use Test;

# Found via the Crane distribution (t/patch.rakutest): a multi sub called from
# a closure with a captured sigilless parameter lost the caller's container.

plan 3;

multi sub assign-one(\c, $v) { c = $v }
multi sub assign-one(\c, Str $v) { c = $v }

sub via-map(\c) { (1,).map({ assign-one(c, 5) }).eager; c }

my $x = 0;
my $r := $x;
via-map($r);
is $x, 5, 'multi sub writes through a sigilless param captured by map closure';

my $y = 0;
via-map($y);
is $y, 5, 'same with a plain variable argument';

sub via-str(\c) { (1,).map({ assign-one(c, 'a') }).eager; c }
my $z = 0;
via-str($z);
is $z, 'a', 'candidate selection by type still works';
