use Test;

plan 29;

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

# The numeric surface. A punned `Rational[Int,Int]` used to carry a numerator
# and a denominator with no way to say what number it IS: `.Str` fell through
# to the type-object rendering, so JSON::Fast's `Rational` branch emitted
# "Rational[Int,Int]()" where rakudo emits 0.3.
my $third = Rational[Int, Int].new(3, 10);
my $neg   = Rational[Int, Int].new(-7, 2);

is $third.Str, '0.3', 'a punned Rational stringifies as its value';
is $neg.Str, '-3.5', 'and so does a negative one';
is $third.Num, 0.3e0, '.Num is the quotient';
is $third.Numeric, 0.3, '.Numeric is the exact quotient';
is $third.Rat, 0.3, '.Rat is the exact quotient';
is $third.Bridge, 0.3e0, '.Bridge bridges to Num';
is $neg.Int, -3, '.Int truncates toward zero, it does not floor';
is $neg.abs, 3.5, '.abs';
is $neg.floor, -4, '.floor';
is $neg.ceiling, -3, '.ceiling';
is $third + 1, 1.3, 'arithmetic goes through the numeric coercion';
ok $third < 1, 'and so does comparison';
dies-ok { Rational[Int, Int].new(1, 0).Str },
    'a zero denominator still dies when coerced to Str';
