use Test;

plan 11;

# A decimal string too long for an i64 numifies to the exact Rat, as the
# numeric literal of the same spelling does, not to an f64.
my $s = '0.1234567890123456789012345';
isa-ok $s.Numeric, Rat, 'Str.Numeric of a long decimal is a Rat';
is-deeply $s.Numeric.nude, (246913578024691357802469, 2000000000000000000000000),
    'with the exact numerator and denominator';
is-deeply "-$s".Numeric.nude, (-246913578024691357802469, 2000000000000000000000000),
    'a negative one too';
is +"0.12345678901234567890", 0.1234567890123456789, 'prefix + keeps every digit';
is "99999999999999999999.5".Numeric.raku, '99999999999999999999.5', 'a long integer part';

# Numeric comparison with a digit string compares at that exact value.
ok 0.1234567890123456789012345 == $s, '== with a long decimal string';
is 0.1234567890123456789012345 <=> $s, Same, '<=> with a long decimal string';
nok FatRat.new(1, 3) == '0.3333333333333333333333333', 'a FatRat differing in the last digits';
my $digits = '1.' ~ '4' x 2000;
ok FatRat.new(('1' ~ '4' x 2000).Int, 10 ** 2000) == $digits, 'a 2000-digit FatRat equals its digits';
ok '0x10' == 16, 'a radix string still numifies';
is '10' <=> '9', More, 'two strings compare numerically';
