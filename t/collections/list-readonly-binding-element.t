use Test;

# A List built from a readonly scalar binding holds that binding's VALUE: a
# name bound straight to a value (`:=`, sigilless) or a readonly parameter
# has no container for the List to alias, so the element is immutable and
# the binding is never boxed into a writable cell. mutsu#10893.

plan 6;

my $b := "lit";
my $l = ($b, 1);
throws-like { $l[0] = 5 }, Exception,
    message => 'Cannot modify an immutable List ((lit 1))',
    'an element from a `:=`-bound value is immutable';
is $b, 'lit', 'and the binding keeps its value';

sub f($p) { my $m = ($p, 1); $m[0] = 5 }
throws-like { f(3) }, Exception,
    message => 'Cannot modify an immutable List ((3 1))',
    'an element from a readonly parameter is immutable';

my $c := "lit";
sub g(&c) { c() }
g({ my $n = ($c, 1); try { $n[0] = 5 } });
is $c, 'lit', 'a closure capture of the binding is not written either';

# A container still aliases.
my ($x, $y);
($x, $y) = (1, 2);
is $x + $y, 3, 'list assignment to variables still works';

my $v = 1;
my $w = ($v, 2);
$w[0] = 10;
is $v, 10, 'an element from a variable aliases its container';
