use Test;

# `for @a.pairs -> $p { $p.value = X }` writes through to the array element, but
# must NOT demote the array to a List: the rebuilt array keeps its Array /
# Shaped / typed identity (previously it became a bare `(...)` List).

plan 7;

{
    my @a = 1, 2, 3;
    for @a.pairs -> $p { $p.value = 42 }
    is-deeply @a, [42, 42, 42], 'plain array stays an Array';
}
{
    my @a = 1, 2, 3;
    for @a.pairs -> $p { $p.value = $p.value * 10 }
    is-deeply @a, [10, 20, 30], 'computed writeback stays an Array';
}
{
    my @a[3];
    for @a.pairs -> $p { $p.value = 7 }
    is @a.raku, 'Array.new(:shape(3,), [7, 7, 7])', '1D shaped array keeps its shape';
    is @a.shape.gist, '(3)', 'shape preserved';
}
{
    my Int @a = 1, 2, 3;
    for @a.pairs -> $p { $p.value = 9 }
    is @a.WHAT.gist, '(Array[Int])', 'typed array keeps its element type';
}
{
    # only the touched index changes
    my @a = 10, 20, 30;
    for @a.pairs -> $p { $p.value = $p.value + 1 if $p.key == 1 }
    is-deeply @a, [10, 21, 30], 'selective writeback';
}
# hash .pairs writeback still works
{
    my %h = a => 1, b => 2;
    for %h.pairs -> $p { $p.value = 9 }
    is-deeply %h, {a => 9, b => 9}, 'hash .pairs writeback unaffected';
}
