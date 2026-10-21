use Test;

# `Int.bits`, `Int.power-up` and `Int.power-down` are declared by Rakudo's Int
# (#14134); `.^methods` lists them and each answers as Rakudo does.

plan 22;

ok Int.^methods.map(*.name).Set<bits power-up power-down>.all, "Int.^methods lists bits, power-up, power-down";

is Int.bits, Inf, "Int.bits of the type object is Inf";
throws-like { 5.bits }, X::Parameter::InvalidConcreteness, "Int.bits refuses an instance";

is 8.power-up(3), 512, "power-up is the power";
is 2.power-up(0), 1, "a zero exponent gives 1";
is (-8).power-up(3), -512, "a negative base keeps its sign";
is 2.power-up(100), 2**100, "power-up reaches big integers";
isa-ok 2.power-up(-1), Failure, "a negative exponent is a Failure";
isa-ok 2.power-up(-1).exception, X::Numeric::Overflow, "...of X::Numeric::Overflow";

isa-ok 8.power-down(3), Failure, "a positive exponent leaves no integer power down";
isa-ok 8.power-down(3).exception, X::Numeric::Underflow, "...X::Numeric::Underflow";
is 2.power-down(-2), 0.25, "a negative exponent gives the reciprocal power";
is 3.power-down(-2), 1/9, "...as a Rat";
isa-ok 3.power-down(-2), Rat, "the result is a Rat";
is 2.power-down(0), 1.0, "a zero exponent gives 1.0";
isa-ok 2.power-down(0), Rat, "...a Rat";
is 1.power-down(5), 1.0, "one raised to any exponent is 1.0";
is 10.power-down(-3), 0.001, "10.power-down(-3)";
isa-ok 2.power-down(-100), Num, "a denominator past 64 bits turns into a Num";
is 2.power-down(-64), 2e0 ** -64, "...with the power as value";
dies-ok { Int.power-up(2) }, "power-up refuses a type object";
dies-ok { Int.power-down(2) }, "power-down refuses a type object";
