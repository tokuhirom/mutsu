use Test;

# Like the arithmetic operators (t/types/string/str-arith-failure.t), the
# numeric comparisons, the integer bitwise/shift operators, gcd/lcm and the
# reductions over them evaluate to a lazy X::Str::Numeric Failure when a Str
# operand cannot be numified, the way rakudo does -- they used to numify the
# bad string to 0 and answer silently (`"a" < 2` was True).

plan 32;

for (
    '<'   => { "a" < 2 },
    '>'   => { "a" > 2 },
    '<='  => { "a" <= 2 },
    '>='  => { "a" >= 2 },
    '≤'   => { "a" ≤ 2 },
    '≥'   => { "a" ≥ 2 },
    '=='  => { "a" == 0 },
    '< (right)'  => { 1 < "a" },
    '+|'  => { "a" +| 1 },
    '+&'  => { "a" +& 1 },
    '+^'  => { "a" +^ 1 },
    '+<'  => { "a" +< 1 },
    '+>'  => { "a" +> 1 },
    '+| (right)' => { 1 +| "a" },
    'gcd' => { "a" gcd 2 },
    'lcm' => { "a" lcm 2 },
    'gcd (right)' => { 2 gcd "a" },
    '[+]' => { [+] "a", 1 },
    '[*]' => { [*] "a", 2 },
    '[+] (right)' => { [+] 1, "a" },
    '[+|]' => { [+|] "a", 1 },
    '[gcd]' => { [gcd] "a", 2 },
    '[lcm]' => { [lcm] "a", 2 },
    '&infix:<+|>' => { &infix:<+|>("a", 1) },
    '&infix:<gcd>' => { &infix:<gcd>("a", 2) },
) -> (:key($op), :value(&code)) {
    my $r := code();
    isa-ok $r, Failure, "$op with a non-numeric Str is a Failure";
    $r.so;
}

is ("a" < 2).exception.^name, 'X::Str::Numeric', 'the Failure wraps X::Str::Numeric';

# `!=` is `not ==`: the Failure is falsy, so the answer is a plain True.
is-deeply "a" != 0, True, '!= negates the == Failure into a Bool';
# A chained comparison reduction short-circuits on the falsy Failure.
is-deeply ([<] "a", 1), False, '[<] with a non-numeric Str is False';

# Numeric strings keep working.
is-deeply "1" < 2, True, 'numeric Str compares numerically';
is-deeply " 3 " == 3, True, 'surrounding whitespace is trimmed';
is "0x10" +| 1, 17, 'radix Str in a bitwise op';
is "4" gcd 6, 2, "numeric Str in gcd";
