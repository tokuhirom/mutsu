use Test;

# A Complex coerces to a Real type (`.Int`, `.UInt`, `.Num`, `.Rat`, `.FatRat`,
# `.Real`) when its imaginary part is `≅ 0` under `$*TOLERANCE` (default 1e-15,
# strict `<`, absolute against zero); otherwise it throws X::Numeric::Real.
# Regression (#11795): `.Int`/`.UInt` only accepted an exactly-zero imaginary
# part, and `.Real`/`.Rat`/`.FatRat` hardcoded 1e-15 instead of reading
# `$*TOLERANCE`.

plan 47;

# --- default tolerance: a negligible imaginary part is dropped -------------
my $c = 3.7 + 1e-20i;
is $c.Int,       3,      '(3.7+1e-20i).Int is 3';
is $c.UInt,      3,      '(3.7+1e-20i).UInt is 3';
is $c.Num,       3.7e0,  '(3.7+1e-20i).Num is 3.7e0';
is $c.Rat,       3.7,    '(3.7+1e-20i).Rat is 3.7';
is $c.FatRat,    3.7,    '(3.7+1e-20i).FatRat is 3.7';
is $c.Real,      3.7e0,  '(3.7+1e-20i).Real is 3.7e0';
isa-ok $c.Int,    Int,    '.Int answers an Int';
isa-ok $c.UInt,   Int,    '.UInt answers an Int';
isa-ok $c.Num,    Num,    '.Num answers a Num';
isa-ok $c.Rat,    Rat,    '.Rat answers a Rat';
isa-ok $c.FatRat, FatRat, '.FatRat answers a FatRat';
isa-ok $c.Real,   Num,    '.Real answers a Num';
is (3.7 + 1e-20i).Int + 1, 4, 'the negligible-imaginary Int is a plain Int in arithmetic';
is Int(3.7 + 1e-20i),      3, 'the Int() coercion type goes through the same check';
is (-3.5 - 1e-20i).Int,   -3, 'a negative real part truncates towards zero';
is (0 + 1e-20i).Int,       0, 'a zero real part with a negligible imaginary part is 0';
is (1e-20 + 1e-21i).Int,   0, 'both parts negligible';

# --- the real part keeps its own failure modes -------------------------------
isa-ok (NaN + 1e-20i).Int, Failure, 'a NaN real part fails as NaN.Int does';
isa-ok (Inf + 1e-20i).Int, Failure, 'an Inf real part fails as Inf.Int does';
throws-like { (-3.5 - 1e-20i).UInt }, X::OutOfRange,
    'a negative real part is out of range for .UInt';

# --- a significant imaginary part is not Real --------------------------------
for <Int Num Rat FatRat> -> $m {
    throws-like { (3.7 + 1e-14i)."$m"() }, X::Numeric::Real,
        :target(::($m)), :message("Cannot convert 3.7+1e-14i to $m: imaginary part not zero"),
        "(3.7+1e-14i).$m throws X::Numeric::Real naming $m";
}
throws-like { (3.7 + 1e-14i).UInt }, X::Numeric::Real,
    :target(Int), :message('Cannot convert 3.7+1e-14i to Int: imaginary part not zero'),
    '.UInt names Int, the type the coercion attempted';
isa-ok (3.7 + 1e-14i).Real, Failure, '.Real answers a Failure, not an exception';
throws-like { (1 + 1e-15i).Int }, X::Numeric::Real,
    'an imaginary part exactly at the tolerance is not negligible (strict <)';
throws-like { (1 - 2i).Int }, X::Numeric::Real, :message('Cannot convert 1-2i to Int: imaginary part not zero'),
    'a negative imaginary part renders with its sign';

# --- $*TOLERANCE = 0: nothing is `≅ 0`, not even a pure real ------------------
{
    my $*TOLERANCE = 0;
    throws-like { (3 + 1e-20i).Int }, X::Numeric::Real, '$*TOLERANCE = 0: .Int rejects a tiny imaginary part';
    throws-like { (3 + 0i).Int },     X::Numeric::Real, '$*TOLERANCE = 0: .Int rejects even 3+0i';
    throws-like { (3 + 0i).UInt },    X::Numeric::Real, '$*TOLERANCE = 0: .UInt rejects 3+0i';
    throws-like { (3 + 0i).Num },     X::Numeric::Real, '$*TOLERANCE = 0: .Num rejects 3+0i';
    throws-like { (3 + 0i).Rat },     X::Numeric::Real, '$*TOLERANCE = 0: .Rat rejects 3+0i';
    throws-like { (3 + 0i).FatRat },  X::Numeric::Real, '$*TOLERANCE = 0: .FatRat rejects 3+0i';
    isa-ok (3 + 0i).Real, Failure,                      '$*TOLERANCE = 0: .Real fails for 3+0i';
}

# --- a looser $*TOLERANCE accepts a larger imaginary part ---------------------
{
    my $*TOLERANCE = 1;
    is (3.9 + 0.5i).Int,    3,    '$*TOLERANCE = 1: .Int accepts 0.5i';
    is (3.9 - 0.5i).Int,    3,    '$*TOLERANCE = 1: .Int accepts -0.5i';
    is (3.9 + 0.5i).UInt,   3,    '$*TOLERANCE = 1: .UInt accepts 0.5i';
    is (3.9 + 0.5i).Num,    3.9e0, '$*TOLERANCE = 1: .Num accepts 0.5i';
    is (3.9 + 0.5i).Rat,    3.9,  '$*TOLERANCE = 1: .Rat accepts 0.5i';
    is (3.9 + 0.5i).FatRat, 3.9,  '$*TOLERANCE = 1: .FatRat accepts 0.5i';
    is (3.9 + 0.5i).Real,   3.9e0, '$*TOLERANCE = 1: .Real accepts 0.5i';
    throws-like { (3.9 + 2i).Int }, X::Numeric::Real, '$*TOLERANCE = 1: 2i is still too large';
}
{
    my $*TOLERANCE = 0.1;
    throws-like { (3 + 0.2i).Int }, X::Numeric::Real, '$*TOLERANCE = 0.1: 0.2i is too large for .Int';
    throws-like { (3 + 0.2i).Num }, X::Numeric::Real, '$*TOLERANCE = 0.1: 0.2i is too large for .Num';
    is (3 + 0.05i).Int, 3, '$*TOLERANCE = 0.1: 0.05i is negligible for .Int';
}

# --- the tolerance is dynamic: it is restored after the scope ------------------
is (3.7 + 1e-20i).Int, 3, 'the default tolerance applies again after the scope';
