use Test;
plan 28;

# Imaginary literal
is 2i, Complex.new(0, 2), "2i literal";
isa-ok 2i, Complex, "2i is Complex";

# Construction via arithmetic
is (3+4i).WHAT, "(Complex)", "3+4i is Complex";
is (3+4i).re, 3, "real part";
is (3+4i).im, 4, "imaginary part";

# Complex.new
is Complex.new(2, 3), 2+3i, "Complex.new(2,3)";
is Complex.new(0, 0), 0+0i, "Complex.new(0,0)";

# Arithmetic: addition
is (1+2i) + (3+4i), 4+6i, "Complex + Complex";
is (1+2i) + 3, 4+2i, "Complex + Int";
is 3 + 2i, 3+2i, "Int + Imaginary";

# Arithmetic: subtraction
is (5+3i) - (2+1i), 3+2i, "Complex - Complex";
is (3+4i) - 1, 2+4i, "Complex - Int";

# Arithmetic: multiplication
is (1+2i) * (3+4i), -5+10i, "Complex * Complex";
is (2+3i) * 2, 4+6i, "Complex * Int";

# Arithmetic: division
is (4+2i) / (1+1i), 3-1i, "Complex / Complex";

# Negation
is -(3+4i), -3-4i, "unary minus on Complex";

# Methods
is (3+4i).abs, 5, "abs of 3+4i = 5";
is (2+3i).conj, 2-3i, ".conj";
is (1+2i).reals.elems, 2, ".reals returns 2-element list";

# Type conversions
is (5+0i).Int, 5, "Complex.Int with zero im";
ok (3+4i).Num == 3, "Complex.Num returns real part";
isa-ok (3).Complex, Complex, "Int.Complex";

# Truthiness
ok ?(1+0i), "non-zero Complex is truthy";
ok !(0+0i), "zero Complex is falsy";
ok ?(0+1i), "pure imaginary is truthy";

# Equality
ok (3+4i) == (3+4i), "Complex == Complex";
ok (5+0i) == 5, "Complex with zero im == Int";
nok (3+4i) == (3+5i), "different Complex not equal";
