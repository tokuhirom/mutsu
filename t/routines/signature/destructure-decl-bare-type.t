use Test;

# A bare type in a grouped declaration (`my ($a, Any, $b) = ...`) is an
# anonymous typed scalar placeholder: it takes one element from the RHS and
# discards it, type-checked like `Any $`. From the Stats module
# (`my ($q1,Any,$q2) = quartiles($x)`), a Data::Summarizers dependency.

plan 9;

sub quartiles() { (1, 2, 3) }

{
    my ($q1,Any,$q2) = quartiles();
    is $q1, 1, 'element before a bare-type placeholder';
    is $q2, 3, 'element after a bare-type placeholder';
}

{
    my (Str, $x) = <p q>;
    is $x, 'q', 'leading bare type skips the first element';
}

{
    my ($a, Int, $b, $c) = 10, 20, 30, 40;
    is "$a $b $c", '10 30 40', 'bare type in the middle of a longer list';
}

{
    my (Int, $b) := 1, 2;
    is $b, 2, 'bare type under binding';
}

{
    my (@a, Int, $b) = 1, 2, 3;
    is-deeply @a, [1, 2, 3], 'array before a bare type slurps the rest';
}

{
    my (Any, %h) = 1, a => 1;
    is-deeply %h, {a => 1}, 'bare type before a hash';
}

throws-like { my (Int, $b) = "x", 2 }, X::TypeCheck,
    'bare-type placeholder type-checks its element';

{
    my (Any, Any, $z) = 7, 8, 9;
    is $z, 9, 'two bare-type placeholders in a row';
}
