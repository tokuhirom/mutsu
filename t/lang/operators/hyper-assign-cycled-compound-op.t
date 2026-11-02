use Test;

# #15764: `@a «op=« RHS` over a longer right side cycles the array, and each
# cycled step applies the op to the slot's already-updated value. The expected
# values are rakudo's.

plan 9;

{
    my @d = 1, 2;
    @d «+=« (1, 2, 3);
    is @d.join(","), "5,4", '«+=« over a longer right side';
}
{
    my @h = 2, 3;
    @h «*=« (2, 3, 4, 5);
    is @h.join(","), "16,45", '«*=« over a right side twice as long';
}
{
    my @g = 1, 2;
    @g «~=« <a b c d e>;
    is @g.join(","), "1ace,2bd", '«~=« over a right side of five';
}
{
    my @f = 1, 2, 3;
    @f «+=« (10, 20);
    is @f.join(","), "11,22,3", 'a shorter right side leaves the rest alone';
}
{
    my @e = 1, 2;
    @e «=« (7, 8, 9);
    is @e.join(","), "9,8", '«=« keeps the last write per slot';
}
{
    my @a = 1, 2;
    my @r = (@a «+=« (1, 2, 3));
    is @r.join(","), "5,4,5", 'the expression value is the slots, cycled';
    is @a.join(","), "5,4", 'and the array keeps its length';
}
{
    my @a = 1, 2;
    my @r = (@a «=« (7, 8, 9));
    is @r.join(","), "9,8,9", '«=« expression value, cycled';
}
{
    my @a = 1, 2, 3;
    my @r = (@a «+=« (10, 20));
    is @r.join(","), "11,22", 'a shorter right side yields its own length';
}
