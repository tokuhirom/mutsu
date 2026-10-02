use Test;

# A `where` clause on a variable declaration sees the declaring scope's
# lexicals (scalar, array, hash) at top level, in a block and in a routine,
# and runs exactly once per assignment -- the initializer included (#10732).
# The checks are counted through a container the predicate pushes to: a
# predicate's plain scalar *write* to an outer lexical is a separate gap.

plan 15;

my $y = 2;
my @a = 1, 2, 3;
my %h = k => 5;

my $x where { $y > 0 } = 5;
is $x, 5, 'a where block reads an enclosing scalar';
my $xa where { $_ <= @a.elems } = 3;
is $xa, 3, 'and an enclosing array';
my $xh where { $_ == %h<k> } = 5;
is $xh, 5, 'and an enclosing hash';

{
    my $y = 3;
    my @seen;
    my $x where { @seen.push($y); True } = 5;
    is-deeply @seen, [3], 'inside a block, the where reads the block\'s lexical once';
}

sub f {
    my $y = 4;
    my @seen;
    my $x where { @seen.push($y); True } = 5;
    @seen
}
is-deeply f(), [4], 'inside a routine, the where reads the routine\'s lexical once';

throws-like { my $z where { $y > 10 } = 5 }, X::TypeCheck::Assignment,
    'a where that rejects the initializer still dies';

my @calls;
my $c where { @calls.push($_); True } = 5;
is-deeply @calls, [5], 'the where runs once for the initializer';
$c = 6;
is-deeply @calls, [5, 6], 'and once per later assignment';

my @icalls;
my Int $i where { @icalls.push($_); True } = 3;
is-deeply @icalls, [3], 'a typed where runs once for the initializer';

my @scalls;
subset Counted where { @scalls.push($_); True };
my Counted $s = 1;
is-deeply @scalls, [1], 'a named subset predicate runs once for the initializer';
$s = 2;
is-deeply @scalls, [1, 2], 'and once per later assignment';

sub g {
    my @gcalls;
    my $g where { @gcalls.push($_); True } = 1;
    @gcalls.elems
}
is g(), 1, 'inside a routine, the initializer is checked once';

my $b where { $_ > 0 } := 7;
is $b, 7, 'a bound where-constrained declaration';

my @bound_checks;
my $bound where { @bound_checks.push($_); True } := 7;
is-deeply @bound_checks, [7], 'a bound declaration checks its where once';

my @typed_bound_checks;
my Int $typed_bound where { @typed_bound_checks.push($_); True } := 8;
is-deeply @typed_bound_checks, [8], 'a typed bound declaration also checks once';
