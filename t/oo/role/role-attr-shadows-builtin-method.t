use v6;
use Test;

# The numerator/denominator/nude builtins used to claim EVERY invocant with a
# catch-all (0 / 1 / (0,1)), shadowing same-named attribute accessors on user
# role/class instances — the Rational role prelude's $.numerator read as 0.

role MyRat[::NuT = Int, ::DeT = Int] does Real {
    has NuT $.numerator = 0;
    has DeT $.denominator = 1;
    method nude { self.numerator, self.denominator }
}

# `MyRat` declares no `new`, so the punned parameterisation gets Mu's default
# constructor — named arguments only, exactly as for a class (the builtin
# `Rational` used below does declare `new($nu, $de)`, hence the positional call).
my $r = MyRat[Int,Int].new(numerator => 3, denominator => 10);
is $r.numerator, 3, 'role instance attr numerator wins over builtin';
is $r.denominator, 10, 'role instance attr denominator wins over builtin';
is-deeply $r.nude.List, (3, 10), 'user nude method wins over builtin';

# The builtin Rational prelude itself now works.
my $q = Rational[Int,Int].new(3, 10);
is $q.numerator, 3, 'Rational prelude numerator';
is $q.denominator, 10, 'Rational prelude denominator';

class WithNum {
    has $.numerator = 42;
}
is WithNum.new.numerator, 42, 'class attr numerator accessor still works';

# Builtins keep working on real numeric types.
is (3/10).numerator, 3, 'Rat numerator';
is (3/10).denominator, 10, 'Rat denominator';
is-deeply (3/10).nude.List, (3, 10), 'Rat nude';
# Rakudo's Int does not do Rational, so it has no numerator/denominator.
throws-like { 7.numerator }, X::Method::NotFound, 'Int has no numerator';
throws-like { 7.denominator }, X::Method::NotFound, 'Int has no denominator';
throws-like { (2**70).numerator }, X::Method::NotFound, 'big Int has no numerator';
throws-like { (2**70).denominator }, X::Method::NotFound, 'big Int has no denominator';

# A punned Rational instance is Real: to-json serializes it numerically
# (JSON::Fast t/04-roundtrip.t), not as an opaque string.
{
    use JSON::Fast;
    is to-json([Rational[Int,Int].new(3, 10)], :!pretty), '[0.3]',
        'Rational instance serializes numerically';
    is-deeply from-json(to-json([Rational[Int,Int].new(3, 10)], :!pretty)),
        [0.3], 'Rational roundtrips as Rat';
}

done-testing;
