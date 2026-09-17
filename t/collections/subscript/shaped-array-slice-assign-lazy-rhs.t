use Test;
use lib 'roast/packages/Test-Helpers/lib';
use Test::Util;

# A positional SLICE assignment to a shaped array pulled its ENTIRE RHS into a
# `Vec` before doing anything with it, however many indices the LHS key list
# actually named. For an ordinary finite RHS this is invisible; for an
# infinite lazy one (`loop { ... }` used as an expression is a lazy Seq in
# raku, pulled element-by-element) it hung forever -- worse on a TYPED shaped
# array, whose per-element type check re-ran the constraint's `where` block
# once per pulled element, so it kept "working" instead of spinning empty
# (GitHub #8633).
#
# The plain (unshaped) `Array` slice-assignment path already stayed lazy for
# this exact RHS shape; the fix bounds the shaped/typed path's RHS pull to
# what the LHS key list can ever consume too.

plan 6;

doesn't-hang
    'subset UInt8 of UInt where * < 256; my UInt8 @bytes[4]; @bytes[^4] = (loop { 0 }); say @bytes;',
    'shaped TYPED array slice assign from an infinite lazy RHS terminates',
    :out("[0 0 0 0]\n");

doesn't-hang
    'my @bytes[4]; @bytes[^4] = (loop { 0 }); say @bytes;',
    'shaped UNTYPED array slice assign from an infinite lazy RHS terminates',
    :out("[0 0 0 0]\n");

# Same shape, run in-process (not just the child-process hang guard above) so
# a regression here also shows up as an ordinary assertion failure.
{
    subset UInt8 of UInt where * < 256;
    my UInt8 @bytes[4];
    @bytes[^4] = (loop { 0 });
    is-deeply @bytes.List, (0, 0, 0, 0), 'shaped typed array slice assign result, in-process';
}
{
    my @bytes[4];
    @bytes[^4] = (loop { 0 });
    is-deeply @bytes.List, (0, 0, 0, 0), 'shaped untyped array slice assign result, in-process';
}

# The bounded pull must still type-check every element it actually stores --
# laziness is not an excuse to skip the constraint.
{
    subset UInt8 of UInt where * < 256;
    my UInt8 @bytes[4];
    throws-like { @bytes[^4] = 1, 2, 999, 4 }, Exception,
        'a finite RHS violating the element constraint still throws';
}

# A plain (non-shaped) array slice assignment from the same lazy RHS keeps
# working -- the reference behavior the shaped/typed path now matches.
{
    my @a;
    @a[^5] = (loop { 0 });
    is-deeply @a.List, (0, 0, 0, 0, 0), 'plain array slice assign from an infinite lazy RHS';
}

done-testing;
