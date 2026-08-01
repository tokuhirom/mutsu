use v6;
use Test;

# A shaped array is fixed-size: `:delete` empties a slot, it cannot shorten the
# array. Deleting the last assigned slot used to collapse the whole array --
# the trailing-hole trim (right for an unshaped array, where `my @a; @a[0] = 1;
# @a[0,1]:delete` really does leave `[]`) ran on shaped arrays too, so the
# slots and the shape metadata went with it.
#
# A slot that held nothing also deletes to `Nil`, not to the `Any` hole an
# unshaped out-of-range delete answers with.

plan 36;

# --- One dimension --------------------------------------------------------
{
    my @z[3];
    @z[2] = 3;
    is-deeply (@z[2]:delete), 3, 'deleting the last assigned slot answers its value';
    is-deeply @z, Array.new(:shape(3,), [Any, Any, Any]), 'the array keeps all three slots';
    is @z.elems, 3, 'and its element count';
}
{
    my @y[3];
    @y[0] = 1;
    is-deeply (@y[0, 1]:delete), (1, Nil), 'a slice reports the never-assigned slot as Nil';
    is-deeply @y, Array.new(:shape(3,), [Any, Any, Any]), 'and the array is unchanged in size';
    is @y.elems, 3, 'element count survives a slice delete';
}
{
    my @u[3];
    is-deeply (@u[0..2]:delete), (Nil, Nil, Nil), 'deleting an entirely empty shaped array';
    is-deeply @u, Array.new(:shape(3,), [Any, Any, Any]), 'leaves it intact';
}
{
    my @n[3];
    @n[1] = 5;
    is-deeply (@n[*]:delete), (Nil, 5, Nil), 'a whatever slice mixes values and Nil';
    is-deeply @n, Array.new(:shape(3,), [Any, Any, Any]), 'and empties every slot in place';
}

# A delete that strands no trailing hole was always right -- it is the trailing
# case that reached the trim.
{
    my @w[3] = 1, 2, 3;
    is-deeply (@w[1]:delete), 2, 'deleting a middle slot';
    is-deeply @w, Array.new(:shape(3,), [1, Any, 3]), 'leaves the neighbours alone';
    is @w.elems, 3, 'and the element count';
}

# --- Nil is about the slot, not about the type object ---------------------
{
    my @z[3];
    @z[0] = Any;
    is-deeply (@z[0]:delete), Any, 'an explicitly assigned Any is not a hole';
}
{
    my @y[3];
    @y[0] = 1;
    @y[0]:delete;
    is-deeply (@y[0]:delete), Nil, 'deleting an already-deleted slot answers Nil';
}
{
    my Int @t[3];
    is-deeply (@t[0]:delete), Nil, 'a typed shaped array is no different';
    my Int @t2[3];
    @t2[0] = 5;
    is-deeply (@t2[0]:delete), 5, 'until the slot holds something';
    is-deeply @t2, Array[Int].new(:shape(3,), [Int, Int, Int]), 'and it keeps its shape';
}

# An unshaped array still trims, and reports an absent slot as the Any hole.
{
    my @a;
    @a[0] = 1;
    is-deeply (@a[0, 1]:delete), (1, Any), 'an unshaped slice answers Any for the absent slot';
    is-deeply @a, [], 'and the array trims away';
}
{
    my @b = 1, 2, 3;
    is-deeply (@b[9]:delete), Any, 'an out-of-range delete answers Any';
    is-deeply @b, [1, 2, 3], 'and changes nothing';
}

# --- More than one dimension ---------------------------------------------
{
    my @m[2;2];
    is-deeply (@m[0;1]:delete), Nil, 'an unassigned 2-D slot answers Nil';
    is-deeply @m, Array.new(:shape(2, 2), [Any, Any], [Any, Any]), 'and the rows stay full';
    is @m.elems, 2, 'the row count survives';
    is-deeply @m[0;1], Any, 'and the slot is still readable';
}
{
    my @m[2;2];
    @m[0;1] = 7;
    is-deeply (@m[0;1]:delete), 7, 'an assigned 2-D slot answers its value';
    is-deeply @m, Array.new(:shape(2, 2), [Any, Any], [Any, Any]), 'and the row keeps its width';
    is-deeply @m[0;1], Any, 'the emptied slot reads as Any';
    is-deeply @m[0;0], Any, 'and its neighbour is untouched';
}

# An unshaped nested array is not fixed-size, so its rows do trim.
{
    my @c = [1, 2], [3, 4];
    is-deeply (@c[1;1]:delete), 4, 'deleting the last element of an unshaped row';
    is-deeply (@c[1;0]:delete), 3, 'then the one before it';
    is-deeply @c, [[1, 2], []], 'trims the row away';
}

# --- Shape rendering survives --------------------------------------------
{
    my @s[3];
    @s[0] = 1;
    @s[0]:delete;
    is @s.raku, 'Array.new(:shape(3,), [Any, Any, Any])', '.raku still names the shape';
    my int @i[3];
    is @i.raku, 'array[int].new(:shape(3,), [0, 0, 0])',
        'a native element type is the lowercase array[int]';
    my Str @st[2];
    is @st.raku, 'Array[Str].new(:shape(2,), [Str, Str])',
        'a boxed one is Array[Str]';
}
