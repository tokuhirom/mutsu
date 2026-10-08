use Test;

# From the Native::Overflow ecosystem distribution: a user `subset int8`
# shares its name with a native width. A store through a for-loop alias
# (a ContainerRef cell) must type-check against the subset, not wrap.

plan 3;

my subset int8 of Int where -128 <= $_ <= 127;
my int8 $c;
my $err;
for $c, 1000 -> \x, $v {
    CATCH { default { $err = .^name; next } }
    x = $v;
}
is $err, 'X::TypeCheck::Assignment', 'out-of-range store via \\x alias throws';

my int8 $d;
for $d, 5 -> \x, $v { x = $v }
is $d, 5, 'in-range store via alias works';

my uint8 @u;
my $q := @u[0];
$q = 257;
is @u[0], 1, 'real native widths still wrap through a bound cell';
