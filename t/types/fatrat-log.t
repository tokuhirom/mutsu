use Test;

plan 6;

# Math::NumberTheory 0.1.4's real-digits converts its input to FatRat and
# calls .log($base), so FatRat must participate in the numeric log dispatch.
my $value = FatRat.new(12, 1).log(e);
isa-ok $value, Num, 'FatRat.log returns a Num';
ok ($value - 2.4849066497880004).abs < 1e-12, 'FatRat.log computes the natural logarithm';
my $absolute = abs(-12.FatRat);
isa-ok $absolute, FatRat, 'free abs preserves FatRat';
is $absolute, 12.FatRat, 'free abs returns the positive FatRat';
ok ((1 / 10**30).FatRat).log(2).WHAT === Num,
    'log accepts FatRat values with large denominators';
ok 1.FatRat.round(10 ** -100).defined, 'round accepts a large FatRat scale';
