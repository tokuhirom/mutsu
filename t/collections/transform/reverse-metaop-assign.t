use Test;

plan 6;

# The reverse meta-operator assignment `$x R op= $y` assigns to its RIGHT
# operand: `$y = $y op $x`. The left operand is left unchanged.

{
    my ($x, $y) = <a b>;
    $x R~= $y;
    is $x, 'a', 'R~= leaves the left operand unchanged';
    is $y, 'ba', 'R~= assigns $y ~ $x to the right operand';
}

{
    my ($x, $y) = (10, 3);
    $x R-= $y;
    is $y, -7, 'R-= computes $y - $x into the right operand';
    is $x, 10, 'R-= leaves the left operand unchanged';
}

# Expression context: the assignment value is the new right-operand value.
{
    my ($x, $y) = (10, 3);
    my $r = ($x R-= $y);
    is $r, -7, 'R-= in expression context returns the assigned value';
}

# The right operand must be a container.
{
    my $foo = 'foo';
    dies-ok { $foo R~= 'foo' }, 'R~= dies when the right operand is a literal';
}
