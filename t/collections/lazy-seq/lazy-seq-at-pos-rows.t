use Test;

# A list with no settled shape (a lazy or unreified `Seq`, a `gather`) reads
# its element through `Seq`'s `AT-POS`/`EXISTS-POS` rows (ADR-11276 slice 5),
# and a `Match` answers `replace-with` from its own row.

plan 9;

my $l = (1..Inf).map(* * 2);
is $l.AT-POS(3), 8, 'AT-POS of an infinite lazy Seq';
ok $l.EXISTS-POS(5), 'EXISTS-POS of an infinite lazy Seq';

my $s = (1, 2, 3).map(* + 1);
is $s.AT-POS(1), 3, 'AT-POS of an unreified Seq';
nok $s.EXISTS-POS(7), 'EXISTS-POS past its end';

my @a = lazy 1..*;
is @a.AT-POS(2), 3, 'AT-POS of a lazy Array';
is (gather { take 1; take 2 }).AT-POS(1), 2, 'AT-POS of a gather';
is (1, 2, 3).Seq.AT-POS("1"), 2, 'a Str index is numified';
is (1, 2, 3).Seq.AT-POS(1.5), 2, 'a Rat index is truncated';

"abcd" ~~ /(b)(c)/;
is $/.replace-with("X"), 'aXd', 'Match.replace-with';
