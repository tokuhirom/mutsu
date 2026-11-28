use Test;

# A grep/map block that cannot observe its topic's container binds the
# element's plain value (#19323). These pin that the answers are unchanged,
# including for elements an earlier grep turned into shared cells, and that a
# block that does write `$_` still writes through.

plan 14;

my @a = 1..10;
is-deeply @a.grep({ $_ %% 3 }), (3, 6, 9), 'grep with a pure predicate';
is-deeply @a.grep({ $_ %% 3 }).map({ $_ * 2 }), (6, 12, 18), 'grep then map';
# The first grep shared its matched slots with its result; a second pass reads
# them the same way.
is-deeply @a.grep({ $_ %% 3 }).map({ $_ * 2 }), (6, 12, 18), 'again, over shared slots';
is-deeply @a.grep({ $_ !%% 3 }).elems, 7, '!%% in a pure predicate';

my $n = 0;
$n += @a.grep({ $_ %% 2 }).map({ $_ + 1 }).elems for ^3;
is $n, 15, 'repeated chains count the same';

for @a.grep({ $_ > 8 }) { $_++ }
is-deeply @a, [1, 2, 3, 4, 5, 6, 7, 8, 10, 11], 'grep result still aliases the source';

my @b = 1..5;
@b.map({ $_ *= 10 }).eager;
is-deeply @b, [10, 20, 30, 40, 50], 'a map block that writes $_ writes through';

my @c = 1..5;
@c.grep({ $_++; True }).eager;
is-deeply @c, [2, 3, 4, 5, 6], 'a grep block that writes $_ writes through';

throws-like { (1, 2).map({ $_ = 5 }).eager }, Exception,
    'assigning the topic of an immutable source still dies';

is-deeply [1, 2, 3].map({ $_ }), (1, 2, 3), 'a block returning its topic';
is-deeply (1..6).grep({ $^a %% 2 }), (2, 4, 6), 'a placeholder block';

my $x = 5;
my @d = ($x, 1);
is-deeply @d.grep({ $_ == 5 }), (5,), 'an element held in a variable';

my $s = 'outer';
$_ = 'caller';
is-deeply <a b>.map({ $_ ~ $s }), ('aouter', 'bouter'), 'outer variables are read';
is $_, 'caller', 'the caller topic is restored';
