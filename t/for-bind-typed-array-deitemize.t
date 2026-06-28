use Test;

# A `for … -> @a` binding de-itemizes the chunk element. When that element is an
# element-typed array (`array[int]`), the binding must preserve the element type
# rather than collapsing it to an untyped `Array` (which `.list` does). This also
# guards against cross-type contamination when the iterated list mixes element
# types: each iteration's `@a` must reflect its own element's type, not a stale
# previous-iteration type.

plan 7;

# Single typed array, bound and inspected.
{
    my int8 @j = 1, 2, 3;
    for ($@j,) -> @a {
        is @a.WHAT.^name, 'array[int8]', 'for-bound typed array keeps element type';
    }
}

# Mixed element types in the iterated list: no cross-iteration contamination.
{
    my int @i;
    my int8 @j;
    my @t = $@i, array[int], $@j, array[int8];
    my @seen;
    for @t -> @a, $T {
        @seen.push: @a.WHAT.^name ~ '=' ~ $T.^name;
    }
    is @seen[0], 'array[int]=array[int]',   'int row binds array[int]';
    is @seen[1], 'array[int8]=array[int8]', 'int8 row binds array[int8] (no contamination)';
}

# Reassignment inside the loop body keeps the bound element type.
{
    my int @i;
    my int8 @j;
    my @t = $@i, array[int], $@j, array[int8];
    my @kinds;
    for @t -> @a, $T {
        @a = 1, 2, 3;
        @kinds.push: @a.WHAT.^name;
    }
    is @kinds[0], 'array[int]',  'int row reassignment keeps array[int]';
    is @kinds[1], 'array[int8]', 'int8 row reassignment keeps array[int8]';
}

# De-itemization still flattens (the PR #3716 `[$@n]` regression must not return).
{
    my @n = 1, 2, 3, 4;
    for ($@n, Any) -> @a, $b {
        is @a.elems, 4, 'itemized array flattens to its elements, not [$@n]';
    }
}

# A plain (untyped) array element still binds as an untyped Array.
{
    for ([1, 2, 3],) -> @a {
        is @a.WHAT.^name, 'Array', 'untyped array element binds as untyped Array';
    }
}
