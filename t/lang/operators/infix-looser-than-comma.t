use Test;
use lib 't/lib-looser-than-comma';

# An infix declared looser than the comma operator takes the whole comma list
# as its right operand (#18094; the annotations distribution declares its
# operators relative to the list-infix levels).

plan 7;

{
    sub infix:<lc>($a, +@b) is looser<,> { "lc($a; {@b.join(',')})" }
    is (1 lc 2, 3), 'lc(1; 2,3)', 'is looser<,>: the right operand is the comma list';
    my @r = 1 lc 2, 3, 4;
    is-deeply @r, ['lc(1; 2,3,4)'], 'assigned to an array, it is one element';
}

{
    sub infix:<lr>($a, +@b) is looser(&infix:<,>) { "lr($a; {@b.join(',')})" }
    is (1 lr 2, 3), 'lr(1; 2,3)', 'is looser(&infix:<,>) is the same level';
}

{
    sub infix:<tc>($a, $b) is tighter<,> { "tc($a;$b)" }
    is-deeply (1 tc 2, 3), ('tc(1;2)', 3), 'is tighter<,>: the comma list holds the operation';
}

{
    sub infix:<ec>($a, +@b) is equiv<,> { "ec($a)" }
    is (1 ec 2), 'ec(1)', 'is equiv<,> parses';
}

{
    use LooserComma;
    is (1 graf 2, 3), 'graf(1; 2,3)', 'an imported operator keeps its looser<,> level';
    is (1 graf 2), 'graf(1; 2)', 'with a single right operand too';
}
