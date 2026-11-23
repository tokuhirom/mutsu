use Test;

plan 10;

# rakudo stores through `AT-POS`, and a defined scalar's (or Pair's)
# `AT-POS(0)` is its own container: every `[0]` over it in a chained store
# addresses the same container again (#18960).

{
    my @c = 1, 2;
    @c[0][0][0] = 9;
    is-deeply @c, [9, 2], 'a [0][0] tail over an Int element';
}
{
    my @c = 1, 2;
    @c[0][0][0][0] = 7;
    is-deeply @c, [7, 2], 'a longer [0] tail';
}
{
    my @c = [1, 2], 3;
    @c[1][0][0] = 9;
    is-deeply @c, [[1, 2], 9], 'through an outer index';
}
{
    my @c = [1, 2],;
    @c[0][0][0] = 9;
    is-deeply @c, [[9, 2],], 'the scalar reached at the last level';
}
{
    my %h = a => 1;
    %h<a>[0][0] = 5;
    is-deeply %h, {a => 5}, 'through a hash root';
}
{
    my $x = 5;
    $x[0][0][0] = 9;
    is $x, 9, 'a scalar variable holding the value';
}
{
    my @c = (a => 1), 2;
    @c[0][0][0] = 9;
    is-deeply @c, [9, 2], 'a Pair element';
}
{
    my @c = 1, 2;
    throws-like { @c[0][0][1] = 9 }, X::OutOfRange,
        message => 'Index out of range. Is: 1, should be in 0..0',
        'another index is out of range';
    is-deeply @c, [1, 2], 'and stores nothing';
}
{
    my @c;
    @c[1][2][3] = 9;
    is @c[1][2][3], 9, 'an undefined path still autovivifies';
}
