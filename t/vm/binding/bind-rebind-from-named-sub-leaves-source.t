use Test;

# A named sub rebinding a captured mainline lexical that was itself `:=`-bound
# to another variable re-points that lexical only: the variable it was bound
# to keeps its contents, and the mainline sees the new binding (#9416). The
# declarations are deliberately at mainline, not in blocks: a mainline sub's
# free variables are resolved through the compunit lexical store (ADR-0024),
# which is the path under test. Same-scope and closure rebinds are
# t/vm/binding/bind-rebind-alias-leaves-source.t (#9357).

plan 14;

my @a = 1, 2;
my @b := @a;
sub rebind-array { @b := [9] }
rebind-array();
is @a, [1, 2], 'array: the old source keeps its contents';
is @b, [9], 'array: the mainline sees the new binding';

my @c = 1;
my @d := @c;
sub rebind-then-grow { @d := [9] }
rebind-then-grow();
@d.push(3);
@c.push(4);
is @c, [1, 4], 'array: later writes to the old source stay there';
is @d, [9, 3], 'array: later writes through the rebound name stay there';

my @e = 1, 2;
my @f := @e;
my @g = 5;
sub rebind-to-var { @f := @g }
rebind-to-var();
@f.push(6);
is @e, [1, 2], 'array rebound to a variable: the old source is untouched';
is @g, [5, 6], 'array rebound to a variable: it shares the new source';

my %h = a => 1;
my %i := %h;
sub rebind-hash { %i := { z => 1 }; %i<w> = 2 }
rebind-hash();
is-deeply %h, { a => 1 }, 'hash: the old source keeps its contents';
is-deeply %i.sort.list, (w => 2, z => 1), 'hash: the mainline sees the new binding';

my $x = 1;
my $y := $x;
sub rebind-scalar { my $n = 42; $y := $n }
rebind-scalar();
$y = 43;
is $x, 1, 'scalar: the old source keeps its value';
is $y, 43, 'scalar: the mainline sees the new binding';

my $s = 1;
my $d := $s;
sub rebind-scalar-value { $d := 0 }
{ my $d = 9; rebind-scalar-value() }
is "$s $d", '1 0', 'scalar rebound from a sub called under a shadowing block';

my @m = 1;
my @n := @m;
sub rebind-in-closure { my &c = { @n := [8] }; c() }
rebind-in-closure();
is "{@m} / {@n}", '1 / 8', 'a closure inside the sub rebinds the mainline lexical';

my @w = 1;
my @z := @w;
sub rebind-after-mainline-rebind { @z := [2] }
@z := [5];
rebind-after-mainline-rebind();
is "{@w} / {@z}", '1 / 2', 'mainline rebind first, then the sub rebinds again';

my @plain = 1, 2;
sub rebind-unbound { @plain := [9] }
rebind-unbound();
is @plain, [9], 'an unbound mainline array is still rebound by the sub';
