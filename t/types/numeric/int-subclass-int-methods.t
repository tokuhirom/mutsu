use Test;

plan 28;

# An instance of a user subclass of Int inherits Int's methods and Int's
# `++` / `--` candidates (#9906).
class MyInt is Int {}

{
    my $x = MyInt.new(5);
    $x++;
    is $x, 6, 'postfix ++ on an Int subclass steps its value';
    isa-ok $x, Int, '... and leaves an Int';
    my $y = MyInt.new(5);
    ++$y;
    is $y, 6, 'prefix ++ on an Int subclass';
    my $z = MyInt.new(5);
    $z--;
    is $z, 4, 'postfix -- on an Int subclass';
    my @a = MyInt.new(1), MyInt.new(2);
    @a[0]++;
    is @a.join(','), '2,2', '++ on an Int-subclass array element';
}

{
    my $x = MyInt.new(5);
    is $x.succ, 6, '.succ answers on the payload';
    is $x.pred, 4, '.pred answers on the payload';
    ok $x.is-prime, '.is-prime answers on the payload';
    is MyInt.new(-3).abs, 3, '.abs';
    is-approx MyInt.new(2).sqrt, sqrt(2), '.sqrt';
    is $x.base(2), '101', '.base (one argument)';
    isa-ok $x.Int, MyInt, '.Int returns the invocant itself';
    isa-ok $x.Numeric, MyInt, '.Numeric returns the invocant itself';
    is $x.WHAT.^name, 'MyInt', '.WHAT still names the subclass';
    is $x.polymod(2), '1 2', '.polymod';
    is $x.fmt('%03d'), '005', '.fmt';
    is sprintf('%x', MyInt.new(255)), 'ff', 'sprintf formats the payload';
}

# Operators take the Int candidates, so a mixed operation stays exact.
{
    my $x = MyInt.new(6);
    isa-ok $x + 1, Int, 'Int subclass + Int is an Int';
    isa-ok 1 + $x, Int, 'Int + Int subclass is an Int';
    isa-ok $x * 2, Int, 'Int subclass * Int is an Int';
    isa-ok $x / 4, Rat, 'Int subclass / Int is a Rat';
    isa-ok MyInt.new(1) / MyInt.new(0), Rat, 'dividing by a zero Int subclass is a Rat, not a Num failure';
    is abs(MyInt.new(-6)), 6, 'the abs() routine takes the payload';
    is ($x max 3).WHAT.^name, 'MyInt', 'max returns the Int subclass operand';
    my $c = MyInt(42);
    is $c.succ, 43, 'an instance built by coercion (MyInt(42)) answers on its payload';
    is $c + 1, 43, '... and operates as it';
}

# Rakudo's `++` on an Int subclass takes the Int candidate, so a user `.succ`
# on the subclass is not consulted by `++` (but is by an explicit call).
{
    class Cnt is Int { method succ { "custom" } }
    my $c = Cnt.new(3);
    is $c.succ, 'custom', 'a user .succ on an Int subclass wins an explicit call';
    $c++;
    is $c, 4, '... but ++ uses the Int candidate';
}
