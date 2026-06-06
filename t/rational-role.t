use Test;

plan 16;

# Parametric Rational role: a user class can compose `does Rational[...]`.
my class MyRat does Rational[Int, Int] {};

is-deeply MyRat.new(6, 4).nude, (3, 2), 'Rational role normalizes via gcd';
is-deeply MyRat.new(42, 0).nude, (1, 0), 'zero denominator keeps sign';
is-deeply MyRat.new(0, 0).nude, (0, 0), 'zero/zero stays zero/zero';
is-deeply MyRat.new(-42, 42).nude, (-1, 1), 'negative numerator reduces';
is-deeply MyRat.new(0, 5).nude, (0, 1), 'zero numerator reduces denominator to 1';

# Subclass of a Rational-composing class is still a Rational.
my class SubRat is MyRat {};
my $o = SubRat.new(42, 31337);
ok $o ~~ (SubRat & MyRat & Rational), 'subclass instance smartmatches all three';
is-deeply $o.nude, (42, 31337), 'coprime numerator/denominator unchanged';

# .Bool / .so dispatch to the role-supplied Bool method.
ok  MyRat.new(3, 5).Bool, 'nonzero numerator is True';
nok MyRat.new(0, 5).Bool, 'zero numerator is False';
nok MyRat.new(0, 5).so,   '.so honors user Bool';
ok  MyRat.new(3, 5).so,   '.so honors user Bool (true)';
nok MyRat,                'Rational type object is falsy';

# Int-subclass payload: `class Foo is Int` carries its integer value, and the
# Rational role preserves the numerator/denominator types.
my class Foo is Int {};
class Bar does Rational[Foo, Foo] {};
my $b = Bar.new(Foo.new(10), Foo.new(20));
is-deeply $b.numerator,   Foo.new(1), 'numerator keeps NuT type';
is-deeply $b.denominator, Foo.new(2), 'denominator keeps DeT type';
is Foo.new(10) gcd Foo.new(4), 2, 'gcd reads Int-subclass payload';

# Numeric coercion of Cool builtins.
is-deeply Duration.new(42).Rat, <42/1>, 'Duration.Rat is exact';
