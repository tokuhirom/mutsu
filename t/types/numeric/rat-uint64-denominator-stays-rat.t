use v6;
use Test;

# From Rat::Precise: rakudo's Rat has a uint64 denominator, so a result whose
# denominator is in 2**63 .. 2**64 - 1 stays a Rat; only a larger one is a Num.

plan 14;

isa-ok (1/2)**62, Rat, 'a 2**62 denominator is a Rat';
isa-ok (1/2)**63, Rat, 'a 2**63 denominator stays a Rat';
isa-ok (1/2)**64, Num, 'a 2**64 denominator degrades to Num';
isa-ok (1/3)**40, Rat, '3**40 < 2**64 stays a Rat';
isa-ok (1/3)**41, Num, '3**41 > 2**64 is a Num';
isa-ok (2/1)**-63, Rat, 'a negative exponent builds the same denominator';

is (.5**63).raku, '0.000000000000000000108420217248550443400745280086994171142578125',
    'a terminating big-denominator Rat prints as the decimal';
is ((1/2)**63 * 2).raku, '0.00000000000000000021684043449710088680149056017398834228515625', 'and after arithmetic';
is-deeply (1/2)**63 == 1/9223372036854775808, True, 'equal to the literal quotient';
is-deeply (1/18446744073709551615).raku, '<1/18446744073709551615>', 'a non-terminating one keeps the fraction form';
isa-ok (1/9223372036854775808), Rat, 'a literal with a 2**63 denominator is a Rat';
isa-ok (1/18446744073709551616), Num, 'a literal with a 2**64 denominator is a Num';
isa-ok (1/2**62) / 2, Rat, 'dividing into the 2**63 range stays a Rat';
isa-ok (1/2**62) / 4, Num, 'and past 2**64 it is a Num';
