use Test;

plan 9;

{
    my $list = ($, 'b');
    is $list[0].VAR.^name, 'Scalar', 'anonymous scalar keeps its container in a List';
    $list[0] = 'c';
    is $list.gist, '(c b)', 'writing through the anonymous scalar changes the List';
    is $list[0].VAR.^name, 'Scalar', 'the container survives a write';
}

{
    my $value = 1;
    is ($value, 2)[0].VAR.^name, 'Scalar', 'literal List reflects a named scalar cell';
    my $list = ($value, 2);
    is $list[0].VAR.^name, 'Scalar', 'stored List reflects the same cell';
    $list[0] = 3;
    is $value, 3, 'the List element aliases the scalar';
    is $list[1].VAR.^name, 'Int', 'a plain List value has no container';
}

{
    my $list = (1, 2);
    is $list[0].VAR.^name, 'Int', 'a List of plain values has no element cells';
    throws-like { $list[0] = 3 }, X::Assignment::RO,
        'a plain List element is still immutable';
}
