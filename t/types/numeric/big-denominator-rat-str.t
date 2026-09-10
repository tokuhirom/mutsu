use v6;
use Test;

# `Rat` is `Rational[Int, uint64]` in rakudo, so an arithmetic result whose
# denominator does not fit `uint64` degrades to `Num`. `Rational.Str` reaches
# its fractional part through such an operation, which means a `Rat` with an
# over-64-bit denominator renders the f64 value re-expanded to the digit budget
# -- NOT the exact expansion, and NOT f64's own 17-digit shortest round-trip,
# which is what mutsu used to print (GH #7577).
#
# `FatRat` is `Rational[Int, Int]` and never degrades, so it keeps the exact
# expansion; that contrast is the discriminator this file pins.

plan 27;

# --- the reported repro ------------------------------------------------
{
    my $a = .1234567890123456789012345;
    is $a.numerator,   246913578024691357802469,  'the literal parses exactly (numerator)';
    is $a.denominator, 2000000000000000000000000, 'the literal parses exactly (denominator)';
    is $a.raku, '<246913578024691357802469/2000000000000000000000000>', '.raku stays exact';
    is $a.Str,  '0.1234567890123456824475648', '.Str re-expands the f64 to the digit budget';
    is $a.gist, '0.1234567890123456824475648', '.gist agrees with .Str';
    is $a.^name, 'Rat', 'a big-denominator decimal literal is a Rat';
}

# --- a nonzero integer part --------------------------------------------
{
    my $a = 1.1234567890123456789012345;
    is $a.Str, '1.1234567890123456824475648', 'nonzero integer part keeps the same fraction digits';
}

# --- the sign case -----------------------------------------------------
{
    my $a = -1.1234567890123456789012345;
    is $a.^name, 'Rat', 'negating a big-denominator Rat literal stays a Rat';
    is $a.Str, '-1.1234567890123456824475648', 'the negated literal renders like its positive twin';
    is $a.numerator, -2246913578024691357802469, 'the negated literal keeps its exact numerator';
}
{
    my $a = 1.1234567890123456789012345;
    is (-$a).^name, 'Rat', 'prefix:<-> on a big-denominator Rat stays a Rat';
    is (-$a).Str, '-1.1234567890123456824475648', 'prefix:<-> renders like the literal';
}

# --- FatRat never degrades ---------------------------------------------
{
    my $f = FatRat.new(246913578024691357802469, 2000000000000000000000000);
    is $f.Str, '0.1234567890123456789012345', 'FatRat keeps the exact expansion';
    is (-$f).^name, 'FatRat', 'prefix:<-> on a FatRat stays a FatRat';
    is (-$f).Str, '-0.1234567890123456789012345', 'a negated FatRat is still exact';
}

# --- the uint64 boundary is where the degradation starts ---------------
{
    # 2^64 - 1 still fits, so these digits are the EXACT expansion.
    my $r = Rat.new(12345678901234567891, 18446744073709551615);
    is $r.Str, '0.669260594276348691851', 'a denominator of 2^64-1 keeps the exact expansion';
}
{
    # 2^64 does not fit, so these digits come from f64.
    my $r = Rat.new(12345678901234567891, 18446744073709551616);
    is $r.Str, '0.669260594276348592128', 'a denominator of 2^64 renders through f64';
}
{
    my $f = FatRat.new(12345678901234567891, 18446744073709551616);
    is $f.Str, '0.669260594276348691814265', 'the FatRat twin of that denominator is exact';
}

# --- a denominator that fits 64 bits must not change -------------------
{
    my $a = 0.1234567890123456789;
    is $a.Str, '0.1234567890123456789', 'a 19-digit denominator is unchanged';
    is $a.^name, 'Rat', 'and is still a Rat';
}
is (1/3).Str,  '0.333333', 'the small-denominator six-digit budget is unchanged';
is (1/7).Str,  '0.142857', 'the small-denominator budget applies to 1/7 too';
is (1/2).Str,  '0.5',      'a terminating small Rat is unchanged';

# --- a fraction that rounds up to the next integer ---------------------
is Rat.new(99999999999999999999999999, 100000000000000000000000000).Str, '1',
   'a Rat whose f64 fraction is 1.0 renders as the next integer';
is FatRat.new(99999999999999999999999999, 100000000000000000000000000).Str,
   '0.99999999999999999999999999',
   'the FatRat twin keeps every nine';

# --- a fraction that underflows f64 -----------------------------------
is Rat.new(1, 10**400).Str, '0',
   'a Rat whose fraction underflows f64 renders as its whole part';

# --- a long but representable fraction --------------------------------
is Rat.new(1, 10**40).Str, '0.' ~ '0' x 39 ~ '1',
   'a 41-digit denominator still renders its digits, not a Num';
