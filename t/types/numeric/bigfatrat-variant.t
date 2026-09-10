use Test;

# A rational whose numerator or denominator overflows i64 is stored as a big
# integer pair. mutsu must still distinguish a (raku) Rat from a (raku) FatRat
# for that case: type name, .WHAT, .Str digit budget, .raku form, ~~, .WHICH,
# eqv, and the .Rat / .FatRat coercions all consult the FatRat flag.

plan 24;

my $bf = (4.5.FatRat) ** 60;   # big FatRat
my $br = 4.5 ** 60;            # big Rat, same numeric value

# --- type identity ---
is $bf.^name, 'FatRat', 'big FatRat reports FatRat';
is $br.^name, 'Rat', 'big Rat reports Rat';
is $bf.WHAT.gist, '(FatRat)', 'big FatRat .WHAT is (FatRat)';
is $br.WHAT.gist, '(Rat)', 'big Rat .WHAT is (Rat)';

# --- smartmatch against the two types ---
ok $bf ~~ FatRat, 'big FatRat ~~ FatRat';
nok $bf ~~ Rat, 'big FatRat is not a Rat';
ok $br ~~ Rat, 'big Rat ~~ Rat';
nok $br ~~ FatRat, 'big Rat is not a FatRat';
ok $bf ~~ Rational, 'big FatRat ~~ Rational';
ok $br ~~ Rational, 'big Rat ~~ Rational';

# --- .Str digit budget: Rat rounds, FatRat keeps full precision ---
my $full = '1558657976916843360832062017400788597510.058834953945635510598466400011830046423710882663726806640625';
my $rounded = '1558657976916843360832062017400788597510.0588349539456355106';
is ~$bf, $full, 'big FatRat .Str keeps full expansion';
is ~$br, $rounded, 'big Rat .Str rounds to the digit budget';

# --- .raku form ---
ok $bf.raku.starts-with('FatRat.new('), 'big FatRat .raku is FatRat.new(..)';
is $br.raku, $full, 'big Rat .raku is the exact terminating decimal';

# --- coercions flip the flavour ---
is $bf.Rat.^name, 'Rat', '.Rat on a big FatRat drops the FatRat flag';
is $br.FatRat.^name, 'FatRat', '.FatRat on a big Rat sets the FatRat flag';
is ~$bf.Rat, $rounded, '.Rat on a big FatRat then rounds like a Rat';
is ~$br.FatRat, $full, '.FatRat on a big Rat then keeps full precision';

# --- WHICH carries the flavour ---
ok $bf.WHICH.Str.starts-with('FatRat|'), 'big FatRat WHICH starts with FatRat|';
ok $br.WHICH.Str.starts-with('Rat|'), 'big Rat WHICH starts with Rat|';

# --- eqv distinguishes the flavour even at equal value ---
nok ($bf eqv $br), 'big FatRat is not eqv to a big Rat of equal value';
ok ($br eqv (4.5 ** 60)), 'two equal big Rats are eqv';
ok ($bf eqv ((4.5.FatRat) ** 60)), 'two equal big FatRats are eqv';

# --- numeric equality still ignores the flavour ---
ok ($bf == $br), 'big FatRat == big Rat of equal value';
