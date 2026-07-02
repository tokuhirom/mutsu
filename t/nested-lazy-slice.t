use Test;

# `lazy LIST` must wrap the WHOLE comma-separated list, not just the first
# term (parser bug: `lazy 3,4,5` was parsing as `(3.lazy, 4, 5)`).
{
    my $x = (lazy 3, 4, 5);
    is $x.is-lazy, True, 'lazy on a finite list literal reports is-lazy True';
    dies-ok { $x.elems }, '.elems on a lazy-marked list throws (matches Rakudo)';
    is $x[2], 5, 'the full 3-item comma list survives (index 2 unreachable if truncated)';
}
{
    my @idx := (0...*).cache;
    is @idx.is-lazy, True, '.cache on an infinite sequence stays lazy when := bound';
    is @idx[200000], 200000, 'lazy-bound .cache reifies on out-of-cache index';
}

# Nested lazy sublist as a read-side index element: distribution stops at
# the container's boundary instead of filling with Any past the end.
{
    my @a = 1 .. 5;
    is-deeply @a[1, (lazy 3, 4, 5)], (2, (4, 5)), 'nested lazy sublist read stops at array boundary';
    is-deeply @a[lazy 1, 2, (4, 5)], (2, 3, (5, Any)), 'plain nested sublist inside a lazy list does not stop early';
}

# Nested lazy sublist as an assignment/bind target.
{
    my @a = 11 .. 15;
    is-deeply (@a[1, (lazy 3, 4, 5)] = "a" ... *), ("a", ("b", "c")),
        'assigning through a nested lazy sublist returns the nested shape';
    is-deeply @a, [11, "a", 13, "b", "c"],
        'assigning through a nested lazy sublist stops writes at the boundary';
}
{
    my @a = 11 .. 15;
    is-deeply (@a[1, (lazy 3, 4, 5)] := "a" ... *), ("a", ("b", "c")),
        'binding through a nested lazy sublist returns the nested shape';
    is-deeply @a, [11, "a", 13, "b", "c"],
        'binding through a nested lazy sublist stops writes at the boundary';
}

# Top-level lazy index list (not wrapped in an outer Array): a plain nested
# sublist inside it still autoviv-extends normally.
{
    my @a = 11 .. 15;
    is-deeply (@a[lazy 1, 2, (4, 5)] = "a" ... *), ("a", "b", ("c", "d")),
        'assigning through a top-level lazy list with a plain nested sublist';
    is-deeply @a, [11, "a", "b", 14, "c", "d"],
        'the plain nested sublist extends the array past its original length';
}

# Duplicate indices (direct leaf appearing both inside a plain nested
# sublist and again at the top level): the top-level occurrence is bounded
# by the ORIGINAL (pre-assignment) length, even though the nested sibling
# already grew the array earlier in the same operation. The returned value
# reflects each position's FINAL state (a post-write read), not the value
# seen at write time.
{
    my @a = 11 .. 15;
    is-deeply (@a[lazy 1, 2, (4, 5), 4, 5] = "a" ... *), ("a", "b", ("e", "d"), "e"),
        'duplicate trailing indices stop at the pre-assignment boundary';
    is-deeply @a, [11, "a", "b", 14, "e", "d"],
        'the nested write is visible, then overwritten once more before the boundary stop';
}

# Flat (non-nested) slice assignment used as a sub-expression (not a bare
# statement) must still write back to the array — a dual-store bug made the
# mutation visible only through the assignment's own return value.
{
    my @a = (1, 2, 3, 4, 5);
    say (@a[1, 3] = "x", "y");
    is-deeply @a, [1, "x", 3, "y", 5],
        'slice assignment used as a say() argument still mutates the array';
}

done-testing;
