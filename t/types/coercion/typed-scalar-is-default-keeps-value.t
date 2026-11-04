use Test;
plan 8;

# `my TYPE $x is default(D)` must start as D and keep its initializer; the
# deferred type registration used to re-seed the slot with the type object.
{
    my Real $p is default(1);
    is $p, 1, 'typed scalar without initializer starts at its default';
}
{
    my Int $r is default(3) = 7;
    is $r, 7, 'typed scalar keeps its initializer';
}
{
    my Real $s is default(1) = 5;
    is $s, 5, 'Real with default and initializer';
}
{
    my Real $product is default(1);
    $product *= 5;
    is $product, 5, 'compound assignment starts from the default';
    $product = Nil;
    is $product, 1, 'assigning Nil restores the default';
}
{
    my Int $n is default(3);
    $n = 9;
    is $n, 9, 'plain assignment';
}
{
    my Str $t is default('x');
    is $t, 'x', 'Str default';
}
{
    my Real @a is default(1);
    is @a[2], 1, 'typed array default unchanged';
}
