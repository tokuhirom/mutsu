use Test;

plan 9;

# An *itemized* list/Range subscript is a SINGLE index (its `.Int`, the element
# count) — container identity makes it one item — not a multi-element slice.
# `@a[$(7,8,9)]` is `@a[3]`, while `@a[7,8,9]` / `@a[(7,8,9)]` stay slices.

{
    my @a;
    @a[$(7,8,9)] = 101;
    is @a.elems, 4,                  'itemized list subscript assigns a single element';
    is @a[$(7,8,9)], 101,            'itemized list subscript reads a single element';
    ok  (@a[$(7,8,9)]:exists),       'itemized list subscript :exists is single';
    @a[$(7,8,9)]:delete;
    nok (@a[$(7,8,9)]:exists),       'itemized list subscript :delete removes the single element';
}

# A bare list / parenthesized list subscript is still a slice.
{
    my @a = ^10;
    is-deeply @a[7, 8, 9],   (7, 8, 9), 'bare list subscript stays a slice';
    is-deeply @a[(7, 8, 9)], (7, 8, 9), 'parenthesized list subscript stays a slice';
    is @a[$(7, 8, 9)], 3,              'itemized list subscript is the element count index';
}

# An itemized Range subscript is also a single index.
{
    my @a = ^10;
    is @a[$( ^3 )], 3,    'itemized Range subscript is a single index';
    is-deeply @a[^3], (0, 1, 2), 'bare Range subscript stays a slice';
}
