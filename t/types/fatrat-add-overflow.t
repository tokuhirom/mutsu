use Test;

plan 6;

my $large = FatRat.new(9223372036854775807, 2);

is ($large + $large).raku,
    'FatRat.new(9223372036854775807, 1)',
    'large FatRat addition promotes before cross-product overflow';
is ($large - $large).raku,
    'FatRat.new(0, 1)',
    'large FatRat subtraction promotes before cross-product overflow';
is ($large + $large).Str,
    '9223372036854775807',
    'large FatRat addition keeps the exact result';

my @phis = (2.FatRat, 1 + 1 / * ... *);
is @phis[200].Str.chars, 50,
    'the golden-ratio FatRat sequence reaches its 200th element';
is @phis[200].^name, 'FatRat',
    'the golden-ratio sequence retains its FatRat type';
is @phis[200].Str, '1.618033988749894848204586834365638117720309179806',
    'the golden-ratio sequence remains exact';
