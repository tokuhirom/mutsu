use v6;
use Test;

# Every form of an integer operator -- the infix, its routine (`&infix:<div>`),
# the reduction (`[div]`), the triangle (`[\div]`), the hyper (`»div»`) and the
# reversed metaop (`Rdiv`) -- is ONE routine (ADR-0118, src/builtins/arith/),
# and so are `++`/`.succ`/`.=succ` and `abs($x)`/`.abs`. Expected values were
# measured with rakudo; each row also asserts the forms agree, so a private
# copy drifting away from the shared routine fails here.

plan 43;

my $min = -9223372036854775808;
my $max =  9223372036854775807;

# -- div: floored, Int-coerced operands, BigInt on overflow --
is 7 div -2, -4, 'div floors';
is ([div] 7, -2), -4, '[div] floors too (it was Euclidean)';
is infix:<div>(7, -2), -4, '&infix:<div>';
is ((7,) »div» -2), (-4,), '»div»';
is (-2 Rdiv 7), -4, 'Rdiv';
is ([\div] 7, -2), (7, -4), '[\\div]';
is 7.5 div 2, 3, 'div truncates a Rat operand';
is "7" div 2, 3, 'div numifies a Str operand';
is $min div -1, 9223372036854775808, 'i64::MIN div -1 promotes (it panicked)';
is ([div] $min, -1), 9223372036854775808, '[div] i64::MIN, -1 promotes';
isa-ok (7 div 0), Failure, 'div by zero is a Failure';

# -- mod: % on the numeric values --
is 7.5 mod 2, 1.5, 'mod keeps a Rat';
is ([mod] 7.5, 2), 1.5, '[mod] agrees';
is -7.5 mod 2, 0.5, 'mod floors a negative Rat';
is -7 mod 3, 2, 'Int mod floors';
is $min mod -1, 0, 'i64::MIN mod -1 is 0 (it panicked)';
is $min % -1, 0, 'i64::MIN % -1 is 0';

# -- bitwise: Int-coerced operands --
is 5.5 +& 3, 1, '+& truncates a Rat (it answered 0)';
is ([+&] 5.5, 3), 1, '[+&] agrees';
is ((5.5, 6.5) »+&» 3), (1, 2), '»+&» agrees';
is 5.5e0 +| 2, 7, '+| truncates a Num';
is "6" +^ 3, 5, '+^ numifies a Str';
is 2**70 +& (2**70 - 1), 0, '+& on BigInts';

# -- shifts --
is 1 +< 64, 18446744073709551616, '+< promotes past 64 bits';
is ([+<] 1, 64), 18446744073709551616, '[+<] agrees';
is -8 +> 1, -4, '+> is arithmetic';
is -1 +> 100, -1, '+> of a negative saturates at -1';
is 8 +< -2, 2, 'a negative +< count shifts right';

# -- negate / abs at i64::MIN --
is -$min, 9223372036854775808, '-i64::MIN is a BigInt (it became a Num)';
isa-ok -$min, Int, '... and an Int';
is $min.abs, 9223372036854775808, '.abs of i64::MIN promotes (it wrapped)';
is abs($min), 9223372036854775808, 'abs() agrees (it panicked)';
is abs(-2**70), 1180591620717411303424, 'abs() of a BigInt (it answered 0)';
is abs("-3"), 3, 'abs() numifies a Str';
is (-1.5).abs, 1.5, '.abs of a Rat';

# -- succ / pred: one routine for .succ, ++ and .=succ --
is $max.succ, 9223372036854775808, '.succ promotes (it wrapped)';
is $min.pred, -9223372036854775809, '.pred promotes (it wrapped)';
is (2**70).succ, 1180591620717411303425, '.succ of a BigInt (it was a no-op)';
is (9223372036854775807/2).succ, 4611686018427387904.5, '.succ of a big Rat stays exact';
is "²".succ, "³", '.succ counts superscript digits, like ++';
{ my $x = "²"; $x++; is $x, "³", '++ agrees' }
isa-ok "a".pred, Failure, '"a".pred is a Failure';
{ my $x = "a"; $x .= pred; isa-ok $x, Failure, '.=pred agrees (it kept the string)' }
