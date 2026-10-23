use v6;
use Test;

# `for @a { ... }` binds `$_` to the element's container. A body that cannot
# tell the container from a copy of its value skips promoting the elements
# (#14495); every body that can must keep the alias. Both sides are pinned.

plan 11;

my @src = 1..10;
my $n = 0;
for @src { $n += $_ }
is $n, 55, 'a read-only scalar-accumulating body sums the elements';

my $max = 0;
for @src { $max = $_ if $_ > $max }
is $max, 10, 'a conditional scalar assignment reads the topic';

my $c = 0;
for @src { $c++ }
is $c, 10, 'a counting body runs once per element';

my @a = 1, 2, 3;
for @a { $_ *= 2 }
is-deeply @a, [2, 4, 6], 'a body that writes $_ still writes through to the array';

my @b = 1, 2, 3;
for @b { $_++ }
is-deeply @b, [2, 3, 4], 'a postfix ++ on the topic writes through';

my @c = 1, 2, 3;
my @seen;
for @c { @c[0] = 99; @seen.push($_) }
is-deeply @seen, [99, 2, 3], 'a body that assigns the source reads the new value through the alias';

my @d = 1, 2, 3;
my @subs;
for @d { my $v = $_; @subs.push({ $v }) }
is-deeply @subs.map({ $_() }).Array, [1, 2, 3], 'a closure over a copy sees the copy';

my @f = <a b c>;
my $s = '';
for @f { $s ~= $_ }
is $s, 'abc', 'string accumulation';

my @g = 5, 6, 7;
my $total = 0;
for @g { my $t = $_ * 2; $total += $t }
is $total, 36, 'a body-local scalar declaration is allowed';

my @h = 1, 2, 3;
my $last;
for @h { $last = $_; last if $_ == 2 }
is $last, 2, '`last` leaves the loop with the second element seen';

my @i = 1, 2, 3;
for @i { $_ = 0 if $_ == 2 }
is-deeply @i, [1, 0, 3], 'a conditional write to the topic is kept';
