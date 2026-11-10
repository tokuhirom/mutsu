use Test;

# `.roll($count)` takes a Rat count like `.pick` does: it truncates toward zero,
# and an infinite Rat (`1/0`) is `*` (ParaSeq's `.roll($elems / 2)`).
plan 9;

my $l = (^20).list;
is $l.roll(10/2).elems, 5, "a whole Rat";
is $l.roll(5.0).elems, 5, "a decimal literal (Rat)";
is $l.roll(5/2).elems, 2, "a fractional Rat truncates";
is $l.roll(7/2).elems, 3, "another one";
is [1, 2, 3].roll(4/2).elems, 2, "on an Array";
is (1..10).roll(6/3).elems, 2, "on a Range";
is <a b c>.roll(3/1).elems, 3, "on a word list";
ok $l.roll(1/0).is-lazy, "an infinite Rat is a lazy roll";
ok $l.roll(5/2).all ∈ $l, "every rolled element is from the list";
