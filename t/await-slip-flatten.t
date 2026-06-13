use Test;

# `await` of multiple promises returns their results as a list. A promise that
# returns a Slip must have its elements flattened into that list (slip
# semantics), while a plain List/Array result stays nested — matching Raku.

plan 6;

{
    my @r = await (start { 1 }), (start { (2, 3).Slip }), (start { 4 });
    is @r.elems, 4, 'slip result flattens into the await list (elems)';
    is-deeply @r, [1, 2, 3, 4], 'slip result flattens into the await list';
}

{
    # A plain List result is NOT a Slip and stays nested.
    my @r = await (start { (2, 3) }), (start { 4 });
    is @r.elems, 2, 'plain list result stays nested (elems)';
    is-deeply @r, [(2, 3), 4], 'plain list result stays nested';
}

{
    # A single promise returning a Slip flattens to its elements.
    my @r = await start { (7, 8, 9).Slip };
    is-deeply @r, [7, 8, 9], 'single slip-returning promise flattens';
}

{
    # Scalar results are unaffected.
    my @r = await (start { 10 }), (start { 20 });
    is-deeply @r, [10, 20], 'scalar results are unaffected';
}
