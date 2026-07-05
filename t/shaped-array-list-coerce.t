use Test;

# `.List`/`.Array` on a shaped array flattens all dimensions, replacing
# uninitialized (Nil) slots with the container's type-default (Any).

plan 8;

{
    my @a[2;2];
    is-deeply @a.List, (Any, Any, Any, Any), '.List on uninited 2x2 shaped';
    is-deeply @a.Array, [Any, Any, Any, Any], '.Array on uninited 2x2 shaped';
}
{
    my @a[4];
    is-deeply @a.List, (Any, Any, Any, Any), '.List on uninited 1D shaped';
}
{
    my @a[2;2] = [1, 2], [3, 4];
    is-deeply @a.List, (1, 2, 3, 4), '.List flattens a populated 2x2';
    is-deeply @a.Array, [1, 2, 3, 4], '.Array flattens a populated 2x2';
}
# .Array returns a fresh copy: mutating it does not touch the shaped array
{
    my @a[3];
    my @b = @a.Array;
    @b[0] = 99;
    is-deeply @a[0], Any, '.Array is a copy, shaped array untouched';
}
# plain arrays / lists are unaffected
{
    my @a = 1, 2, 3;
    is-deeply @a.List, (1, 2, 3), 'plain array .List unaffected';
    is-deeply @a.Array, [1, 2, 3], 'plain array .Array unaffected';
}
