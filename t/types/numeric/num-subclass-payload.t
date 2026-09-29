use Test;

# A user subclass of Num carries its float payload the way an `is Int`
# subclass carries its integer: `Num.new($x)` boxes `$x.Num` into the
# subclass. Regression: the instance had no payload at all, so it stringified
# as `F.new`, added as 0, and `.Real` died with "No such method 'Real'".
# Found by the Math::FractionalPart distribution (`class Math::FractionalPart
# is Num`, `Math::FractionalPart.new($val).modf`).

class F is Num {
    method twice { self * 2 }
}

plan 20;

my $n = F.new(2.5);
isa-ok $n, F, 'Num subclass .new keeps the subclass type';
isa-ok $n, Num, 'a Num subclass instance is a Num';
is $n.Str, '2.5', '.Str is the payload';
is "$n", '2.5', 'interpolation is the payload';
is $n.gist, '2.5', '.gist is the payload';
is $n + 1, 3.5, 'arithmetic uses the payload';
ok $n == 2.5, 'numeric comparison uses the payload';
is $n.twice, 5, 'a user method sees self as the payload';
ok $n.Real === $n, '.Real returns the invocant';
ok $n.Num === $n, '.Num returns the invocant';
ok $n.Numeric === $n, '.Numeric returns the invocant';
is $n.floor, 2, '.floor answers on the payload';
is $n.Int, 2, '.Int answers on the payload';
is F.new(-1.5).abs, 1.5, '.abs answers on the payload';
is F.new(-1.5).sign, -1, '.sign answers on the payload';
is sprintf('%.2f', $n), '2.50', 'sprintf formats the payload';

is F.new.Str, '0', 'no argument is 0e0';
is F.new('-3/2').Str, '-1.5', 'a Str argument numifies like .Num';
is F.new(3).Str, '3', 'an Int argument is boxed as a Num';

my $x = $n;
$x .= Real;
is $x, 2.5, '.= Real keeps the value';
