use Test;

plan 7;

# `my ($a, $b) is default(D) = ...` applies the group trait to every element,
# also when an initializer is present (#12547).
{
    my ($a, $b) is default(7) = 1;
    is $a, 1, 'assigned element keeps its value';
    is $b, 7, 'element the RHS did not reach takes the default';
    $a = Nil;
    is $a, 7, 'later Nil assignment resets to the default';
}

{
    my ($c, @d) is default(3) = 5, 6;
    is $c, 5, 'scalar element assigned';
    is @d.raku, '[6]', 'slurpy array element unaffected';
}

{
    my ($x, $y) is default(2);
    is $x, 2, 'bare form still carries the default';
    is $y, 2, 'bare form, second element';
}
