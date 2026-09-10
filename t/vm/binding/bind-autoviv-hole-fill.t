use Test;

plan 8;

# Binding a scalar to a not-yet-existing array element (`my $r := @a[5]`) and
# assigning through it autovivifies the array. The gap it opens must be filled
# with the element's type object (`Any`, or the declared element type), matching
# Raku — not `Nil`.

{
    my @a;
    my $r := @a[5];
    $r = 1;
    is-deeply @a, [Any, Any, Any, Any, Any, 1], 'bind-autoviv past the end fills Any holes';
}
{
    my @a = 1, 2;
    my $r := @a[5];
    $r = 9;
    is-deeply @a, [1, 2, Any, Any, Any, 9], 'bind-autoviv keeps existing elements, fills Any';
}
{
    my @a;
    my $r := @a[2];
    $r = 1;
    is @a[0].WHAT.^name, 'Any', 'a hole is the Any type object';
    nok @a[0].defined, 'a hole is undefined';
}
{
    my Int @a;
    my $r := @a[3];
    $r = 5;
    is @a[0].WHAT.^name, 'Int', 'a typed array fills holes with its element type';
    is-deeply @a[0 .. 2].map(*.^name), ('Int', 'Int', 'Int'), 'all holes are the element type';
}
# an ordinary autovivifying assignment is unchanged
{
    my @a;
    @a[5] = 1;
    is-deeply @a, [Any, Any, Any, Any, Any, 1], 'plain @a[5] = 1 still fills Any';
}
# binding to an existing element still writes through
{
    my @a = 1, 2, 3;
    my $r := @a[1];
    $r = 99;
    is-deeply @a, [1, 99, 3], 'binding an existing element writes through';
}
