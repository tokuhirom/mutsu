use Test;

plan 9;

# `Z=` (zip metaoperator applied to `=`) is element-wise assignment:
# `@a[i] = rhs[i]` for i < min(len), trailing @a elements keep their value.
# https://github.com/Raku/roast (S03-metaops/zip.t, S02-types/array-shapes.t)

# --- already-declared array -------------------------------------------------
{
    my @a = 0 xx 3;
    @a Z= 1, 2, 3;
    is-deeply @a, [1, 2, 3], 'equal-length Z= fills every element';
}

{
    my @a = 1, 2, 3, 4, 5;
    @a Z= 10, 20;
    is-deeply @a, [10, 20, 3, 4, 5], 'Z= keeps trailing elements past the RHS';
}

{
    my @a = 1, 2;
    @a Z= 10, 20, 30, 40;
    is-deeply @a, [10, 20], 'Z= ignores RHS elements past the LHS';
}

{
    my @a = 0 xx 6;
    @a Z= 10, 20, 30;
    is-deeply @a, [10, 20, 30, 0, 0, 0], 'Z= is not @a = (@a Z rhs) (no truncation of @a)';
}

# --- declaration form -------------------------------------------------------
{
    my @a Z= 0 .. 5;
    is-deeply @a, [], 'my @a Z= ... on a fresh empty array assigns nothing';
}

# --- shaped array fill (the only flat-fill path; `=` is X::Assignment::ToShaped)
{
    my @a[2;3] Z= 0 .. 5;
    is +@a, 2, 'shaped Z= keeps the first dimension';
    is @a[0;1], 1, 'shaped Z= fills row-major (0;1)';
    is @a[1;2], 5, 'shaped Z= fills row-major (1;2)';
}

# --- `Z=>` (zip-pair) must be unaffected by the `Z=` rewrite -----------------
{
    my %h = <a b c> Z=> 1, 2, 3;
    is-deeply %h, {a => 1, b => 2, c => 3}, 'Z=> (zip pair) still builds a hash';
}
