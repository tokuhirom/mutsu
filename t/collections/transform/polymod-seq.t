use Test;

# polymod returns a Seq, not a List (matches Rakudo).
plan 8;

is 10.polymod(3, 3).^name, 'Seq', 'polymod with args returns Seq';
is 10.polymod(5).^name, 'Seq', 'polymod single arg returns Seq';
is 10.polymod.^name, 'Seq', 'polymod no arg returns Seq';

is 10.polymod(3, 3).raku, '(1, 0, 1).Seq', 'polymod raku render is a Seq';
is 120.polymod(10, 10, 10).raku, '(0, 2, 1, 0).Seq', 'polymod multi-divisor render';
is 10.polymod(5).raku, '(0, 2).Seq', 'polymod single-divisor render';

# The values themselves are still correct.
is 1000.polymod(10, 10, 10).List, (0, 0, 0, 1), 'polymod values are correct';

my @d = 2, 2, 2, 2;
is 1000.polymod(@d).List, (0, 0, 0, 1, 62), 'polymod with array divisors';
