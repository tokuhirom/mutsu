use v6;
use Test;

# #9012: the `floor`/`ceiling` FREE FUNCTIONS return `Int` for a boxed `Num`
# argument (a literal, a computed expression, or a `Num`-typed variable) but
# must stay `Num` for a genuinely native `num` argument -- mutsu had no
# runtime tag distinguishing the two, so it always kept `Num`, which matched
# only the native case and broke the (far more common) boxed one. The METHOD
# form (`.floor`/`.ceiling`) is unaffected: it always returns `Int`, matching
# rakudo, because there is no native-container receiver to preserve.

plan 12;

# Boxed cases: rakudo answers Int.
is-deeply ceiling(3.2e0), 4, 'ceiling(literal Num) value';
isa-ok ceiling(3.2e0), Int, 'ceiling(literal Num) type';
is-deeply floor(3.2e0), 3, 'floor(literal Num) value';
isa-ok floor(3.2e0), Int, 'floor(literal Num) type';

my Num $z = 3.2e0;
is-deeply ceiling($z), 4, 'ceiling(Num variable) value';
isa-ok ceiling($z), Int, 'ceiling(Num variable) type';

# A computed boxed Num (the Graph::MinCuttish shape from the issue).
my $n = 10;
isa-ok ceiling(1 + $n / sqrt(2)), Int, 'ceiling(computed expression) type';

# Native cases: rakudo keeps Num.
my num $y = -4.7e0;
is-deeply ceiling($y), -4e0, 'ceiling(native num variable) value';
isa-ok ceiling($y), Num, 'ceiling(native num variable) type';

my num $w = -4.7e0;
is-deeply floor($w), -5e0, 'floor(native num variable) value';
isa-ok floor($w), Num, 'floor(native num variable) type';

# Inline anonymous native declaration (roast S02-types/num.t's own shape).
is-deeply ceiling(my num $ = -4.7e0), my num $ = -4e0, 'ceiling(inline native num) value+type';

done-testing;
