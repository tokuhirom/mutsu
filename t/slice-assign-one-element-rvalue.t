use Test;

# A slice assignment's value is the list it stored, however short: `@a[0,]`
# names a list of one slot, so `(@a[0,] = 5)` is `(5,)`. mutsu used to collapse
# it to the bare element, because the flag that answers "this subscript names
# one key" (needed so an itemized index stays a single key) was also deciding
# the rvalue's shape.

plan 12;

{
    my @d;
    is (@d[0,] = 5).raku, '(5,)', 'a one-element slice yields a one-element list';
}

{
    my @d;
    is (@d[0,] = 5).^name, 'List', 'and its type is List, not the element type';
}

{
    my @d;
    is (@d[(0,)] = 5).raku, '(5,)', 'a parenthesised one-element index list is a slice too';
}

# The controls: a genuine single-index assignment still yields the bare value.
{
    my @d;
    is (@d[0] = 5).raku, '5', 'a single positional index yields the bare value';
}

{
    my %h;
    is (%h<a> = 5).raku, '5', 'a single hash key yields the bare value';
}

{
    my %h;
    is (%h{'a',} = 5).raku, '(5,)', 'a one-element hash slice yields a one-element list';
}

# An *itemized* subscript is one index, not a slice -- that is what the
# single-element flag exists for, and it must keep answering "single".
{
    my @d;
    is (@d[$(0,)] = 5).raku, '5', 'an itemized subscript is a single index, not a slice';
}

{
    my $s = $(1, 2);
    my %c;
    %c{$s} = 'x';
    is %c.keys.elems, 1, 'an itemized hash subscript is still one key';
}

# A multi-element slice is unaffected.
{
    my @d;
    is (@d[0, 1] = 5, 6).raku, '(5, 6)', 'a multi-element slice keeps the flat list';
}

# The slice's own arity decides the rvalue's length, not the RHS's.
{
    my @d;
    is (@d[0,] = 5, 6).raku, '(5,)', 'a one-slot slice reports only the value it stored';
}

{
    my @d;
    @d[0,] = 5, 6;
    is @d.raku, '[5]', 'and it stores only that one slot';
}

# The rvalue flattens into a list assignment as one element.
{
    my @a;
    my @z = (@a[0,] = 1, 2);
    is @z.elems, 1, 'a one-element slice rvalue contributes one element';
}
