use Test;

plan 11;

is (0 + Inf\i).raku, '<0+Inf\i>', 'escaped imaginary suffix with Inf stringifies in .raku';

sub check_default_raku($value, $expected, Mu $raku = $expected) is test-assertion {
    is $value.raku, $raku, 'test-assertion metadata does not override default parameter';
}
check_default_raku 1/2, '0.5';

is '<1/3>', '<1/3>' | '¹/₃', 'is handles RHS junction candidates';

is (0/2).raku, '0.0', '0/2 keeps Rat-style .raku';
is (1/1).raku, '1.0', '1/1 keeps Rat-style .raku';

my $huge = 555555555555555555555555555555555555555555555/5;
is ~$huge, '111111111111111111111111111111111111111111111', 'exact BigInt division keeps full string';
is ~$huge.FatRat, '111111111111111111111111111111111111111111111', 'BigInt .FatRat keeps full string';

my $pow-rat = 4.5 ** 60;
my $pow-rat-str = '1558657976916843360832062017400788597510.058834953945635510598466400011830046423710882663726806640625';
is ~$pow-rat, $pow-rat-str, 'Rat ** Int keeps arbitrary precision stringification';
is $pow-rat.raku, $pow-rat-str, 'Rat ** Int .raku keeps terminating decimal precision';
is ~$pow-rat.FatRat, $pow-rat-str, 'Rat ** Int .FatRat keeps arbitrary precision stringification';
is $pow-rat.FatRat.Str, $pow-rat-str, 'Rat ** Int .FatRat.Str keeps arbitrary precision stringification';
