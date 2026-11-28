use Test;

# `for %h.values { ... }` whose body cannot observe its topic's container
# reads the values without promoting the hash's elements to shared cells, and
# skips the per-iteration topic writeback (#19294). A body that does write
# `$_` still writes through to the hash.

plan 8;

my %h = a => 1, b => 2, c => 3;
my $sum = 0;
for %h.values { $sum += $_ }
is $sum, 6, 'a pure body sums the values';

my $big = 0;
for %h.values { $big++ if $_ > 1 }
is $big, 2, 'a conditional on the topic';

for %h.values { $_ *= 10 }
is-deeply %h.sort.list, (a => 10, b => 20, c => 30), 'a body that writes $_ writes through';

my @seen;
for %h.values { @seen.push($_) }
is-deeply @seen.sort.list, (10, 20, 30), 'a body that passes $_ on';

my %e;
my $n = 0;
for %e.values { $n++ }
is $n, 0, 'an empty hash';

my %t is default(5);
%t<x> = 1;
my $t = 0;
for %t.values { $t += $_ }
is $t, 1, 'a hash with a default';

# The plain read leaves the hash's own elements alone: a later `.values`
# that does alias them still writes through.
for %h.values { $sum += $_ }
for %h.values { $_ = 0 }
is-deeply %h.values.sum, 0, 'aliasing still works after a plain read';

my %k = x => 1;
my $acc = '';
for %k.values { $acc ~= "$_;" }
is $acc, '1;', 'string interpolation of the topic';
