use Test;
plan 29;

# Basic construction via division
isa-ok 1/4, Rat, "Int / Int produces Rat";
isa-ok Rat.new(1, 4), Rat, "Rat.new produces Rat";

# .WHAT
is (1/4).WHAT, "(Rat)", "Rat.WHAT";

# GCD reduction
is (2/4).nude.join(","), "1,2", "2/4 reduces to 1/2";
is (6/9).nude.join(","), "2,3", "6/9 reduces to 2/3";

# Negative sign normalization
is Rat.new(1, -7).nude.join(","), "-1,7", "negative denominator normalizes";
is Rat.new(-3, -5).nude.join(","), "3,5", "double negative normalizes";

# .numerator / .denominator
is (3/4).numerator, 3, ".numerator";
is (3/4).denominator, 4, ".denominator";

# Arithmetic
is 1/4 + 1/4, 1/2, "1/4 + 1/4 = 1/2";
is 3/4 - 1/4, 1/2, "3/4 - 1/4 = 1/2";
is (2/3) * (3/4), 1/2, "2/3 * 3/4 = 1/2";
is (2/3) / (4/3), 1/2, "2/3 / 4/3 = 1/2";

# Rat + Int
is 1/2 + 1, 3/2, "Rat + Int";
is 1/2 - 1, -1/2, "Rat - Int";
is (1/2) * 3, 3/2, "Rat * Int";

# Comparison
ok 1/4 < 1/2, "Rat < comparison";
ok 3/4 > 1/2, "Rat > comparison";
ok 1/2 == 1/2, "Rat == comparison";

# Equality with Int
ok 2/2 == 1, "Rat == Int when equal";
ok 4/2 == 2, "4/2 == 2";

# Negative power
is 2 ** -3, 1/8, "Int ** negative = Rat";

# Type conversions
is (3/2).Int, 1, "Rat.Int truncates";
ok (1/2).Num == 0.5, "Rat.Num";
isa-ok (1/2).Rat, Rat, "Rat.Rat is idempotent";

# Truthiness
ok ?(1/2), "non-zero Rat is truthy";
ok !(0/1), "zero Rat is falsy";

# .isNaN
ok Rat.new(0, 0).isNaN, "0/0 is NaN";
nok (1/2).isNaN, "1/2 is not NaN";
