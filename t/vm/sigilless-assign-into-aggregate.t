use Test;

# Assigning to a sigilless name bound to an Array/Hash stores INTO it
# (`Array.STORE` / `Hash.STORE`), so every other holder sees the change.

plan 8;

my @a = 1, 2;
my \v = @a;
v = 3, 4, 5;
is-deeply @a, [3, 4, 5], 'my \v = @a; v = ... fills @a';

my %h = a => 1;
my \w = %h;
w = b => 2;
is-deeply %h, {b => 2}, 'my \w = %h; w = ... fills %h';

my @b = 1, 2;
for (@b,) -> \x { x = Empty }
is-deeply @b, [], 'single sigilless loop param';

my @c = 1, 2;
my %d = k => 1;
for (1, @c, 2, %d) -> \key, \value { value = Empty }
is-deeply @c, [], 'multi-param sigilless loop param (Array)';
is-deeply %d, {}, 'multi-param sigilless loop param (Hash)';

my @e = 1, 2;
sub fill(\target) { target = 9, 8 }
fill(@e);
is-deeply @e, [9, 8], 'sigilless routine param';

my @f;
my @g = (1, 2), (3, 4);
for @g -> \pair { @f.push: pair }
is-deeply @f, [(1, 2), (3, 4)], 'loop params re-seat, not write into the previous value';

my \l = (1, 2);
throws-like { l = 3 }, X::Assignment::RO, 'a List stays immutable';
