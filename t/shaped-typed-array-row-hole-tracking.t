use Test;

# `ArrayData::hole_at` decides whether an unwritten slot of a TYPED array is a
# gap by comparing the stored type object's name against the array's own
# `value_type`. A multidimensional shaped array's rows are arrays in their own
# right, and only the top-level array was ever tagged -- so every untouched
# `Int` cell of a row reported `:exists == True`.
#
# The 1D typed form and the untyped 2D form were both already correct, which is
# what makes the row the odd one out.
#
# `todo/deep/typed-shaped-array-rows-lose-element-value-type.md` recorded this
# as having "no raku oracle", because raku dies on `@a[0]` for a shaped array
# ("Partially dimensioned views of shaped arrays not yet implemented"). It does
# have one: drop that probe and raku answers every `:exists` row below. All
# expectations here are measured against raku v2026.07.

plan 17;

# The headline: a 2D typed shaped array.
{
    my Int @a[2;2];
    @a[0;0] = 1;
    @a[1;1] = 4;
    ok  @a[0;0]:exists, 'a written cell of a typed 2D row exists';
    nok @a[0;1]:exists, 'an unwritten cell of a typed 2D row is a hole';
    nok @a[1;0]:exists, 'and so is one in another row';
    ok  @a[1;1]:exists, 'while the other written cell exists';
    is (@a[0;0]:kv).raku, '((0, 0), 1)', ':kv agrees';
    is (@a[0;1]:p).raku, '()', ':p on a hole is empty';
    is @a.raku, 'Array[Int].new(:shape(2, 2), [1, Int], [Int, 4])',
        'the stored structure is unchanged';
}

# Three dimensions: the tagging has to recurse, not just cover the first row.
{
    my Int @c[2;2;2];
    @c[0;0;0] = 1;
    nok @c[0;0;1]:exists, 'a hole two dimensions down is a hole';
    ok  @c[0;0;0]:exists, 'and the written cell still exists';
}

# A non-numeric element type takes the same path.
{
    my Str @e[2;2];
    @e[0;0] = 'x';
    nok @e[0;1]:exists, 'a Str row tracks holes too';
}

# Writing a hole fills it; `:delete` turns it back into one.
{
    my Int @a[2;2];
    @a[0;0] = 1;
    @a[0;1] = 2;
    ok @a[0;1]:exists, 'writing the hole fills it';
    @a[0;1]:delete;
    nok @a[0;1]:exists, ':delete turns it back into a hole';
}

# What must NOT change.
{
    my Int @s[3];
    @s[0] = 1;
    nok @s[1]:exists, '1D typed: unchanged';

    my @d[2;2];
    @d[0;0] = 1;
    nok @d[0;1]:exists, 'untyped 2D: unchanged';

    my int @n[2;2];
    @n[0;0] = 1;
    ok @n[0;1]:exists, 'a NATIVE element type has no type object, so every slot exists';

    my @z := Array[Int].new(:shape(2,2));
    @z[0;0] = 1;
    nok @z[0;1]:exists, 'the Array[T].new(:shape(...)) constructor agrees';

    my Int @t[2;2] = ([1,2],[3,4]);
    ok @t[0;1]:exists, 'a fully-initialised shaped array has no holes';
}
