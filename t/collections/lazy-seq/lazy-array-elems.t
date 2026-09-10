use Test;

# A lazy (infinite-backed) array cannot report its element count: raku throws
# X::Cannot::Lazy ("Cannot .elems a lazy list") rather than returning the capped
# backing length.

plan 15;

{
    my @a = 1..*;
    throws-like { @a.elems }, X::Cannot::Lazy, 'lazy array .elems throws X::Cannot::Lazy';
}

{
    my @a = 1..*;
    my $caught;
    my $r = try { @a.elems; CATCH { default { $caught = $_ } } };
    is $caught.message, 'Cannot .elems a lazy list', 'message matches';
}

# the lazy array remains usable for indexing / iteration after the failed .elems
{
    my @a = 1..*;
    is @a[5], 6, 'lazy array still indexable';
    is-deeply @a[^4], (1, 2, 3, 4), 'lazy array slice works';
    ok @a.is-lazy, 'still lazy';
}

# An anonymous bracket-array literal built from a single infinite integer range
# (`[1..Inf]`, `[1..*]`, `[^Inf]`) is lazy too — like `my @a = 1..Inf` — instead
# of being materialized to a MAX_RANGE_EXPAND-capped finite prefix.
{
    throws-like { [1..Inf].elems }, X::Cannot::Lazy, '[1..Inf].elems throws X::Cannot::Lazy';
    throws-like { [1..*].elems },   X::Cannot::Lazy, '[1..*].elems throws X::Cannot::Lazy';
    throws-like { [^Inf].elems },   X::Cannot::Lazy, '[^Inf].elems throws X::Cannot::Lazy';
    ok [1..Inf].is-lazy, '[1..Inf] is lazy';
    is [1..Inf][3], 4, '[1..Inf] still indexable';
    is-deeply [1..Inf][^5], (1, 2, 3, 4, 5), '[1..Inf] slice works';
}

# finite arrays / ranges are unaffected
{
    my @b = 1, 2, 3;
    is @b.elems, 3, 'finite array .elems works';
    is (1..10).elems, 10, 'finite range .elems works';
    my @c = 1..100;
    is @c.elems, 100, 'finite range-assigned array .elems works';
    # a finite bracket-array literal stays eager
    is [2..6].elems, 5, '[2..6].elems (finite bracket array) works';
}
