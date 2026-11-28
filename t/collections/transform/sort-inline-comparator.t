use Test;

# `.sort` with a `{ $^a <=> $^b }` / `{ $^a cmp $^b }`-shaped comparator is
# evaluated without calling the block. Over all-Int or all-Str input it uses
# an adaptive stable sort; over anything else, the stable merge sort. Both
# must give rakudo's order, stable for equal keys.

plan 14;

my @asc = 1..20;
is-deeply @asc.sort({ $^b <=> $^a }).List, (20...1).List, 'descending Int sort of ascending input';
is-deeply @asc.reverse.sort({ $^a <=> $^b }).List, (1..20).List, 'ascending Int sort of descending input';

my @mixed = (^50).map({ ($_ * 37) % 23 - 11 });
is-deeply @mixed.sort({ $^a <=> $^b }).List, @mixed.sort.List, 'Int comparator matches the default sort';
is-deeply @mixed.sort({ $^b <=> $^a }).List, @mixed.sort.reverse.List, 'reversed Int comparator';

# Stability: equal keys keep their input order. Allomorphs compare by value
# but keep their own identity, so the order of equal elements is visible.
my @dups = <3 1 2 1 3>;
is @dups.sort({ $^a <=> $^b }).map(*.WHICH.Str.contains('IntStr')).all.so, True, 'allomorphs keep their type';
is-deeply (1, 2.0, 1.0, 2).sort({ $^a <=> $^b }).map(*.^name).List, <Int Rat Rat Int>.List,
    'equal Int and Rat keys stay in input order';

my @words = <pear apple fig apple banana fig cherry>;
is-deeply @words.sort({ $^a cmp $^b }).List, <apple apple banana cherry fig fig pear>.List, 'Str comparator';
is-deeply @words.sort({ $^b cmp $^a }).List, <pear fig fig cherry banana apple apple>.List, 'reversed Str comparator';
is-deeply <b a c>.sort({ $^a cmp $^b }).List, <a b c>.List, 'short Str list';

# Big Ints, Nums and mixed input take the general comparison.
is-deeply (2**70, 5, -2**65, 0).sort({ $^a <=> $^b }).List, (-2**65, 0, 5, 2**70).List, 'big Ints';
is-deeply (2.5e0, -1e0, 3, 0.5).sort({ $^b <=> $^a }).List, (3, 2.5e0, 0.5, -1e0).List, 'Num, Int and Rat';

# Elements held in Scalar containers.
my @cells = 5, 3, 9;
@cells[1] = 7;
is-deeply @cells.sort({ $^a <=> $^b }).List, (5, 7, 9).List, 'array elements in containers';

# A large sorted run and a large random-looking input.
my @big = (^2000).map({ ($_ * 7919) % 2003 });
is-deeply @big.sort({ $^a <=> $^b }).List, @big.sort.List, 'large Int input';
is-deeply (^3000).list.sort({ $^b <=> $^a })[0, 1, 2999].List, (2999, 2998, 0).List, 'large descending run';
