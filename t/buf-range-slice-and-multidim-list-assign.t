use Test;

plan 11;

# --- A Range subscript is a slice on a Buf, like a comma list ---------------
# The Buf element-assign path handled a comma list (`$b[0,1,2] = …`) and a
# single index but not a Range, so `$b[$i ..^ $i + 8] = @bytes` fell through to
# the scalar arm and reported "Index out of range". Digest::SHA3's `store64`
# write-back is exactly that form.
{
    my buf8 $n .= new: 0 xx 16;
    $n[0 ..^ 8] = 1, 2, 3, 4, 5, 6, 7, 8;
    is $n.list.join(","), "1,2,3,4,5,6,7,8,0,0,0,0,0,0,0,0",
        'an exclusive Range slice assigns through a Buf';

    my $i = 8;
    $n[$i ..^ $i + 8] = 9 xx 8;
    is $n.list.join(","), "1,2,3,4,5,6,7,8,9,9,9,9,9,9,9,9",
        '...at a computed offset';

    my buf8 $m .= new: 0 xx 8;
    $m[0 .. 2] = 1, 2, 3;
    is $m.list.join(","), "1,2,3,0,0,0,0,0", 'an inclusive Range slice too';

    my buf8 $p .= new: 0 xx 8;
    $p[0, 1, 2] = 7, 7, 7;
    is $p.list.join(","), "7,7,7,0,0,0,0,0", 'control: the comma-list slice';

    # The element width still masks the stored value.
    my buf8 $w .= new: 0 xx 4;
    $w[0 .. 1] = 300, -1;
    is $w.list.join(","), "44,255,0,0", 'stored elements are masked to the width';

    # Wider buffers keep their element width.
    my buf16 $b16 .= new: 0 xx 4;
    $b16[0 .. 1] = 300, 70000;
    is $b16.list.join(","), "300,4464,0,0", 'a buf16 range slice keeps 16-bit elements';
}

# --- A multi-dimensional subscript is a list-assignment target --------------
# `($a, @m[$x;$y]) = ...` failed the compiler's list-assignment target gate and
# fell through to the runtime's "cannot assign through non-callable value".
# Digest::SHA3's rho/pi step is written this way.
{
    my @a = [1, 2], [3, 4];
    my $c = 9;
    ($c, @a[0;1]) = @a[0;1], $c;
    is $c, 2, 'the scalar target receives its item';
    is @a.raku, [[1, 9], [3, 4]].raku, 'the multi-dim target receives its item';

    my @e = [1, 2], [3, 4];
    (@e[0;0], @e[1;1]) = 7, 8;
    is @e.raku, [[7, 2], [3, 8]].raku, 'two multi-dim targets in one assignment';

    # The RHS is snapshotted before any target is written, so a swap works.
    my @s = [1, 2], [3, 4];
    (@s[0;0], @s[1;1]) = @s[1;1], @s[0;0];
    is @s.raku, [[4, 2], [3, 1]].raku, 'a swap through two multi-dim targets';

    # Mixed with a slurpy tail.
    my @f = [0, 0], [0, 0];
    my @rest;
    (@f[0;0], @rest) = 1, 2, 3;
    is "{@f[0;0]} {@rest.join(',')}", "1 2,3",
        'a multi-dim target followed by a slurpy array';
}
