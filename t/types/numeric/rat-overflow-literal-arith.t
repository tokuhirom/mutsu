use Test;

# A long decimal literal is an exact Rat whose denominator does not fit in
# 64 bits. Arithmetic on it is plain Rat arithmetic: the result's denominator
# overflows uint64, so it degrades to Num (the default `$*RAT-OVERFLOW`),
# never to FatRat (#11428).

plan 10;

my $r = 0.1234567890123456789012345;
is $r.^name, 'Rat', 'the literal itself is a Rat';
is ($r + 0).^name, 'Num', 'Rat + Int degrades to Num';
is ($r * 1).^name, 'Num', 'Rat * Int degrades to Num';
is ($r + 0.5).^name, 'Num', 'Rat + Rat degrades to Num';
is ($r - 1).^name, 'Num', 'Rat - Int degrades to Num';
is ($r / 3).^name, 'Num', 'Rat / Int degrades to Num';
is ("0.1234567890123456789012345".Numeric + 0).^name, 'Num',
    'Str.Numeric of a long decimal behaves the same';
is-approx $r + 0, 0.12345678901234568e0, 'the degraded value is right';

is (FatRat.new(1, 2**70) + 1).^name, 'FatRat', 'FatRat arithmetic stays FatRat';
{
    my $*RAT-OVERFLOW = FatRat;
    is ($r + 0).^name, 'FatRat', '$*RAT-OVERFLOW = FatRat upgrades instead';
}
