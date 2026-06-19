use Test;

# A lazy (infinite-backed) array cannot report its element count: raku throws
# X::Cannot::Lazy ("Cannot .elems a lazy list") rather than returning the capped
# backing length.

plan 8;

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

# finite arrays / ranges are unaffected
{
    my @b = 1, 2, 3;
    is @b.elems, 3, 'finite array .elems works';
    is (1..10).elems, 10, 'finite range .elems works';
    my @c = 1..100;
    is @c.elems, 100, 'finite range-assigned array .elems works';
}
