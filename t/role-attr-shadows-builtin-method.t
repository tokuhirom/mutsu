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

my $r = MyRat[Int,Int].new(3, 10);
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
is 7.numerator, 7, 'Int numerator';
is 7.denominator, 1, 'Int denominator';
is (2**70).numerator, 2**70, 'BigInt numerator';
is (2**70).denominator, 1, 'BigInt denominator';

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
