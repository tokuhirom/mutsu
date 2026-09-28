use Test;

# A numeric Str numifies the way `.Numeric` does before a Cool numeric
# *method* runs, so a decimal string is a Rat: `"-5.9".abs` is the Rat 5.9.
# Regression: the method form parsed the string as an f64 first, so it was the
# Num 5.9 and `"-5.9".abs - "-5.9".abs.floor` was 0.9000000000000004 instead of
# 0.9. Found by the Math::FractionalPart distribution (t/frac-methods.t).

plan 10;

isa-ok '-5.9'.abs, Rat, '"-5.9".abs is a Rat';
is '-5.9'.abs, 5.9, '"-5.9".abs is 5.9';
is '-5.9'.abs - '-5.9'.abs.floor, 0.9, 'the fractional part is exact';
ok '-5.9'.abs === abs('-5.9'), 'the method form agrees with the function form';
isa-ok '1e2'.abs, Num, 'an exponent string is still a Num';
isa-ok '3'.abs, Int, 'an integer string is still an Int';
isa-ok '1/2'.abs, Rat, 'a rational string is a Rat';
is ' 2.5 '.floor, 2, 'surrounding whitespace is ignored';
is '0x10'.abs, 16, 'a radix string numifies';
is '6+8i'.abs, 10, 'a complex string numifies';
