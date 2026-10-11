use Test;

plan 9;

# A Rat/FatRat invocant keeps its non-zero whole elements rational; a zero
# element is an Int.
is 5.Rat.polymod(.3, .2).raku, '(0.2, 0, 80.0).Seq', 'Rat: last quotient stays a Rat';
is 6.Rat.polymod(2).raku, '(0, 3.0).Seq', 'Rat: whole quotient is a Rat, zero remainder an Int';
is (1/3).polymod(3).raku, '(<1/3>, 0).Seq', 'Rat: zero quotient is an Int';
is FatRat.new(1, 3).polymod(3).raku, '(FatRat.new(1, 3), 0).Seq', 'FatRat invocant keeps its value';
is FatRat.new(7, 2).polymod(2).raku, '(FatRat.new(3, 2), FatRat.new(1, 1)).Seq', 'FatRat elements stay FatRat';
# An Int invocant is unchanged.
is 5.polymod(2.5).raku, '(0, 2).Seq', 'Int invocant with a Rat divisor stays Int';
is 86400.polymod(60, 60, 24).raku, '(0, 0, 0, 1).Seq', 'Int polymod unchanged';
is-deeply 42.Rat.polymod(lazy 2, 3), 42.Rat.polymod(2, 3), 'Rat invocant with a lazy divisor list';
is 42.Rat.polymod(lazy 2, 3).raku, '(0, 0, 7.0).Seq', 'lazy divisors keep the Rat type';
