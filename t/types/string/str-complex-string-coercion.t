use Test;

# `Str.Int`, `.UInt`, `.Num`, `.Rat`, `.FatRat` and `.Real` are
# `self.Numeric.METHOD` in Rakudo, so a string that numifies to a Complex is
# coerced as that Complex: a negligible imaginary part (`$*TOLERANCE`) leaves
# the real part to coerce, any other imaginary part is `X::Numeric::Real` (a
# lazy Failure for `.Real`). mutsu used to truncate the real part for `.Int`
# and answer 0 for `.Num`. Expected values come from `raku`.

plan 31;

# A real imaginary part: not a Real.
for <Int UInt Num Rat FatRat> -> $m {
    throws-like { "1+2i"."$m"() }, X::Numeric::Real, "\"1+2i\".$m is not a Real";
}
{
    my $f = "1+2i".Real;
    isa-ok $f, Failure, '"1+2i".Real is a Failure';
    is $f.exception.^name, 'X::Numeric::Real', 'its exception is X::Numeric::Real';
    $f.so; # handled
}

# The error names the number the string spells, and the type attempted.
sub message-of(&code) {
    my $message = 'no error';
    try { code(); CATCH { default { $message = .message } } }
    $message;
}
is message-of({ "1+2i".Int }), 'Cannot convert 1+2i to Int: imaginary part not zero',
    '.Int names the Complex and Int';
is message-of({ "1+2i".UInt }), 'Cannot convert 1+2i to Int: imaginary part not zero',
    '.UInt names Int, the type attempted';
is message-of({ "1+2i".Num }), 'Cannot convert 1+2i to Num: imaginary part not zero',
    '.Num names Num';

# A negligible imaginary part: the real part is coerced.
is "3+1e-20i".Int, 3, '"3+1e-20i".Int is 3';
is "3+1e-20i".Num, 3e0, '"3+1e-20i".Num is 3e0, not 0';
is "3+1e-20i".Rat, 3.0, '"3+1e-20i".Rat is 3.0';
is "3+1e-20i".Real, 3e0, '"3+1e-20i".Real is 3e0';
is "1.5+0i".Int, 1, '"1.5+0i".Int truncates the real part';
is "1.5+0i".Num, 1.5e0, '"1.5+0i".Num is 1.5e0';
is "-2.5+1e-30i".Num, -2.5e0, 'a negative real part keeps its sign';
throws-like { "-2.5+1e-30i".UInt }, X::OutOfRange, '.UInt still range-checks the real part';

# The tolerance is the dynamic `$*TOLERANCE`, as for a Complex value.
{
    my $*TOLERANCE = 1;
    is "1+0.5i".Int, 1, 'a wide $*TOLERANCE admits a larger imaginary part (.Int)';
    is "1+0.5i".Num, 1e0, 'a wide $*TOLERANCE admits a larger imaginary part (.Num)';
}

# The coercion-type call forms and a variable invocant go the same way.
throws-like { Int("1+2i") }, X::Numeric::Real, 'Int("1+2i")';
throws-like { Num("1+2i") }, X::Numeric::Real, 'Num("1+2i")';
is Int("3+1e-20i"), 3, 'Int("3+1e-20i")';
is Num("3+1e-20i"), 3e0, 'Num("3+1e-20i")';
{
    my $s = "1+2i";
    throws-like { $s.Int }, X::Numeric::Real, 'a Str variable invocant';
}

# Neighbours are unchanged: plain numeric strings, whitespace and U+2212.
is " 3+0i ".Int, 3, 'surrounding whitespace is ignored';
is "−3+0i".Int, -3, 'U+2212 MINUS SIGN is a minus';
is "42".Int, 42, 'a plain integer string';
is "4.5".Num, 4.5e0, 'a plain decimal string';
throws-like { "2i".Int }, X::Numeric::Real, 'a purely imaginary string is not a Real';
is "0i".Num, 0e0, 'a zero imaginary string is the real zero';
