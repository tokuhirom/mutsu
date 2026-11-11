use Test;

# Element stores into an Array a `$` scalar holds (`$d[$i] = $v`,
# `$d[$i] := ...`) have their own fast lane (#17429). These pin the
# semantics it must keep: the write is seen by every holder of the array,
# a bind shares the source's container, a bound element is written through,
# and skipped slots stay holes.

plan 15;

sub g { 'g' }

{
    my @x;
    my $d = @x[0] = [];
    $d[2] = 5;
    is @x[0][2], 5, 'an assignment is seen by another holder of the array';
    $d[0] := &g;
    is @x[0][0](), 'g', '... and so is a bind';
}

{
    my $d = [];
    $d[3] = 1;
    is $d.elems, 4, 'a store past the end grows the array';
    nok $d[1]:exists, '... leaving the skipped slots as holes';
    ok $d[3]:exists, '... while the stored slot exists';
}

{
    my $d = [1, 2];
    $d[1] := $d[0];
    $d[0] = 9;
    is $d[1], 9, 'an element bound to another element aliases it';
    $d[1] = 4;
    is $d[0], 4, '... in both directions';
}

{
    my $x = 1;
    my $d = [];
    $d[0] := $x;
    $d[1] := $x;
    $x = 7;
    is-deeply ($d[0], $d[1]), (7, 7), 'elements bound to one variable follow it';
    $d[1] = 3;
    is $x, 3, '... and an assignment to a bound element writes through';
}

{
    my &c := { 'block' };
    my $d = [];
    $d[$_] := &c for ^3;
    is-deeply $d.map({ .() }).List, <block block block>, 'a bound Block is callable from each element';
    is $d[2](), 'block', '... and by index';
}

{
    my @tables;
    for ^3 -> $id {
        my $t = @tables[$id] = [];
        $t[$_] = $id * 10 + $_ for ^2;
    }
    is-deeply @tables.map(*.List).List, ((0, 1), (10, 11), (20, 21)),
        'a fresh scalar per iteration writes its own array';
}

{
    my $d = [];
    my $r = ($d[0] = 42);
    is $r, 42, 'the assignment yields the stored value';
    my $l = ($d[1] := &g);
    is $l(), 'g', 'the bind yields the bound value';
}

{
    my $d = (1, 2, 3);
    throws-like { $d[0] = 5 }, X::Assignment::RO, 'a List held in a scalar stays immutable';
}
