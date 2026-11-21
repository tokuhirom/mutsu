use Test;

# #18540: the value of an element bind of a variable, `(%h<k> := $x)`, is
# `$x`'s container -- shared with the element -- so binding it aliases `$x`
# and its `.VAR` is a Scalar.

plan 9;

{
    my $y = 5;
    my %h;
    my $r := (%h<p> := $y);
    $r = 9;
    is $y, 9, 'binding the bind expression aliases the variable';
    is %h<p>, 9, 'and the element';
}
{
    my $x = 5;
    my %g;
    is (%g<k> := $x).VAR.^name, 'Scalar', 'the expression is a Scalar';
    is (%g<j> := $x), 5, 'whose value is the variable value';
    (%g<i> := $x) = 7;
    is $x, 7, 'assigning to the expression writes the variable';
}
{
    my $x = 5;
    my @a;
    my $r := (@a[0] := $x);
    $r = 9;
    is $x, 9, 'an array element bind expression aliases too';
    is-deeply @a, [9], 'and the element sees it';
}
{
    my $s = (1, 2).Seq;
    my %g;
    my @l = (%g<k> := $s);
    is @l.elems, 1, 'a Scalar holding a Seq stays one item';
    my %v;
    is-deeply (%v<k> := (1, 2)), (1, 2), 'a bound value without a variable is the value';
}
