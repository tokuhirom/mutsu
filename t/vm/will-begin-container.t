use Test;

plan 5;

# `will begin` runs at BEGIN time with `$_` bound to the declared container,
# so an assignment in it is visible to the mainline.
my $x will begin { $_ = 3 };
is $x, 3, 'will begin assigns to the scalar container';

my @a will begin { .push(1, 2) };
is-deeply @a, [1, 2], 'will begin pushes onto the array container';

my $order;
BEGIN $order ~= 'a';
my $y will begin { $order ~= 'b' };
BEGIN $order ~= 'c';
is $order, 'abc', 'will begin runs in source order among BEGIN phasers';

my $z will begin { $_ = 1 } = 5;
is $z, 5, 'the run-time initializer still applies after will begin';

my $seen;
my $w will begin { $seen = .defined };
is $seen, False, 'the container is in its static state at BEGIN time';
