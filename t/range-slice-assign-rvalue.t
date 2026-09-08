use Test;

# A Range subscript on a plain positional array is a SLICE, so the assignment's
# own value is the list it stored -- a one-element Range yields a one-element
# list, not the bare element, and a short RHS reports the padded list. See
# GitHub issue #7651 (sibling of #7589, which fixed the `@d[0,]` comma
# spelling and had a different root cause).
#
# Every assertion was checked against rakudo, so the file passes there too.

plan 10;

# 1-3. The one-element Range spellings.
{
    my @d;
    is (@d[0..0] = 5).raku, '(5,)', 'an inclusive one-element Range slice yields a one-element list';
}
{
    my @d;
    is (@d[0..^1] = 5).raku, '(5,)', 'the exclusive-end spelling yields a one-element list';
}
{
    my @d;
    is (@d[^1] = 5).raku, '(5,)', 'and so does the ^1 spelling';
}

# 4. A short RHS reports the padded list, not just what was given.
{
    my @d;
    is (@d[0..2] = 5,).raku, '(5, Any, Any)', 'a short RHS over a Range reports the padded list';
}

# 5-6. Controls: a multi-element Range and a plain scalar subscript.
{
    my @d;
    is (@d[0..1] = 5, 6).raku, '(5, 6)', 'a multi-element Range slice is unchanged';
}
{
    my @d;
    is (@d[0] = 5).raku, '5', 'a single-index subscript still yields the bare element';
}

# 7. The comma-list spelling (#7589) is unchanged.
{
    my @d;
    is (@d[0,] = 5).raku, '(5,)', 'the one-element comma slice is unchanged';
}

# 8-10. Storage stays correct, including the short-RHS padding.
{
    my @d;
    @d[0..0] = 5;
    is @d.raku, '[5]', 'a one-element Range slice stores one element';
}
{
    my @d;
    @d[1..2] = 5, 6;
    is @d.raku, '[Any, 5, 6]', 'an offset Range slice stores at the right indices';
}
{
    my @d;
    @d[0..2] = 5,;
    is @d.raku, '[5, Any, Any]', 'a short RHS pads the remaining slots with Any';
}
