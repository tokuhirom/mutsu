use Test;

plan 12;

# A plain Block / pointy block hands back the container its last statement
# denotes; a `sub` decontainerizes (#17094).
my $x = 5;
my &b = { $x };
b() = 7;
is $x, 7, 'assigning to a block call writes through the variable it returns';

my @a = 1, 2;
my &c = { @a[0] };
c() = 9;
is-deeply @a, [9, 2], 'an element tail is assignable through the block call';

my $z = 1;
my &topic = { $_ };
is topic($z).VAR.^name, 'Scalar', '{ $_ } returns the topic container';
topic($z) = 4;
is $z, 4, 'and the topic container is the argument variable';
is { $z }().VAR.^name, 'Scalar', 'a bare block call returns the variable container';

sub g { $z }
is g().VAR.^name, 'Int', 'a sub decontainerizes its result';

my $p = 1;
my $q = 1;
my &id = { $_ };
my @cells = ($p, $q).map({ id($_) });
is @cells.elems, 2, 'a block called from a map callback still maps';

# A non-container tail stays a plain value and is not assignable.
my &v = { $x + 1 };
throws-like { v() = 3 }, X::Assignment::RO, 'a computed tail is not assignable';

# A map callback that calls a container-returning block copies the value
# per iteration, as rakudo's lazy pull-and-copy does.
my $q2 = 1;
my @r = (^3).map({ { $q2 = $q2 + 10; $_ }($q2) });
is @r.join(','), '11,21,31', 'map over a block call returning its topic container';

is (1, 2, 3).map({ $_ * 2 }).join(','), '2,4,6', 'ordinary callbacks are unchanged';

# Native callers read the value, not the container, of a block's tail.
my @pos = 1, 2;
my @sp = 1, 2, 3, 4;
is-deeply @sp.splice({ @pos[0] }, { @pos[1] }), [2, 3], 'splice callable args read the tail value';
is-deeply @sp, [1, 4], 'and splice the right range';
