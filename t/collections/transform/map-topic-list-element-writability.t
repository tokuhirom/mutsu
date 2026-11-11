use Test;

# A `.map`/`.grep` callback's implicit `$_` is the source element: writable
# when that element is a container, immutable when it is a bare value -- also
# when the receiver reaches `.map` through a parameter or a scalar, or the
# callback arrives as a value (#16949, from Iter::Able's t/29-for-each).

plan 10;

sub through-param(\it) { it.map({ --$_ }).sink }
throws-like { through-param((1, 2)) }, X::Multi::NoMatch,
    'a List of literals reached through a sigilless parameter refuses the write';

my &dec = { --$_ };
throws-like { (1, 2).map(&dec).sink }, X::Multi::NoMatch,
    'a callback passed as a value refuses the write on a List of literals';

my $list = (1, 2);
throws-like { $list.map({ --$_ }).sink }, X::Multi::NoMatch,
    'so does a List held in a scalar';

my @a = 1, 2, 3;
through-param(@a);
is-deeply @a, [0, 1, 2], 'an Array reached through a parameter writes through';

my @b = 1, 2;
@b.list.map({ $_ = 5 }).sink;
is-deeply @b, [5, 5], '@a.list writes through';

my ($x, $y) = 1, 2;
my &seven = { $_ = 7 };
($x, $y).map(&seven).sink;
is "$x $y", '7 7', 'a List of containers writes through a callback passed as a value';

my ($p, $q) = 1, 2;
my $held = ($p, $q);
$held.map({ $_ = 8 }).sink;
is "$p $q", '8 8', '... and through a block, when the List is held in a scalar';

my ($m, $n) = 1, 2;
sub write-all(\it) { it.map({ $_ = 9 }).sink }
write-all(($m, $n));
is "$m $n", '9 9', '... and through a parameter';

my $arr = [1, 2];
$arr.map({ $_ *= 10 }).sink;
is-deeply $arr, [10, 20], 'an Array held in a scalar writes through';

my $g = (1, 2);
throws-like { $g.grep({ $_ = 0 }).sink }, X::AdHoc,
    '.grep refuses a write on a List of literals held in a scalar';
