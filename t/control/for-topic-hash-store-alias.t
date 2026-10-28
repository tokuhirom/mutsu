use v6;
use Test;

# A `for @a { %h{$_} = ... }` body stores into a hash element, which cannot
# reach the source array's elements, so the loop skips the per-element alias
# promotion (#7549). Every body that can still tell the container from a copy
# of its value must keep the alias; both sides are pinned.

plan 9;

my @keys = <a b c>;
my %h;
for @keys { %h{$_} = 1 }
is-deeply %h, %(a => 1, b => 1, c => 1), 'a plain element store keyed by the topic';

my %len;
for @keys { %len{$_ ~ 'x'} = $_.chars + 1 }
is-deeply %len, %(ax => 2, bx => 2, cx => 2), 'the key and value expressions read the topic';

my @nums = 1, 2, 3;
my %sq;
for @nums { %sq{$_} = $_ * $_ }
is-deeply %sq, %("1" => 1, "2" => 4, "3" => 9), 'the stored value is the topic read at store time';

my @w = 1, 2, 3;
my %seen;
for @w { %seen{$_} = 1; $_ *= 10 }
is-deeply @w, [10, 20, 30], 'a body that also writes $_ still writes through to the array';
is-deeply %seen, %("1" => 1, "2" => 1, "3" => 1), 'and the store before the write used the old value';

my @v = 1, 2, 3;
my %orig;
for @v { %orig{$_} = $_; @v[1] = 99 }
is-deeply %orig.values.sort.List, (1, 3, 99), 'a body assigning the source reads the new value through the alias';

my @e = 4, 5;
my %cnt;
for @e { %cnt{$_} = %cnt.elems }
is-deeply %cnt, %("4" => 0, "5" => 1), 'the hash is read back inside the body';

my %bound;
my @b = 1, 2;
for @b { %bound{$_} := $_ }
%bound{1} = 7;
is-deeply @b, [7, 2], 'binding the topic into a hash element keeps the alias';

my @m = 1, 2;
my %cap;
for @m { %cap{$_} = { $_ } }
is %cap{1}.(), 1, 'a closure stored as the value still sees its topic';
