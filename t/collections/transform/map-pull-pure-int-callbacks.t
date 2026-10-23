use v6;
use Test;

# A deferred `.map` whose callback is `$_ OP constant` over Ints is computed
# by the operator itself while a consumer pulls it one element at a time
# (#10187). Laziness against a live source and every fallback are pinned.

plan 8;

my @a = 1..6;
my @out;
for @a.map(* + 1) { @out.push($_); @a[4] = 100 if $_ == 3 }
is-deeply @out, [2, 3, 4, 5, 101, 7], 'a write to the live source between pulls is seen';

my @b = 1..4;
my @o2;
for @b.map({ $_ * 2 }) { @o2.push($_); @b.push(9) if @b.elems < 6 }
is-deeply @o2, [2, 4, 6, 8, 18, 18], 'an element appended during the loop is mapped';

my @c = 1, 2.5, 3;
is-deeply @c.map(* + 1).list, (2, 3.5, 4), 'a Rat element runs the real operator';

my @d = 9223372036854775806, 9223372036854775807;
is-deeply @d.map(* + 1).list, (9223372036854775807, 9223372036854775808), 'an i64 overflow promotes';

my $sum = 0;
for (1..5).map({ $_ - 1 }) { $sum += $_ }
is $sum, 10, 'a Range source';

my $it = (10, 20, 30).map(* * 2).iterator;
is $it.pull-one, 20, 'an Iterator pull-one';
is $it.pull-one, 40, '... and the next one';

my @e = <1 2 3>;
is-deeply @e.map(* + 1).list, (2, 3, 4), 'numeric string elements';
