use Test;
use lib $*PROGRAM.parent(2).add("roast/packages/Test-Helpers/lib");
use Test::Util;

# An inclusive `a .. b` subscript whose endpoint arrives through a VARIABLE that
# happens to be negative used to cast `-1` straight to `usize`, so the slice loop
# ran from 0 to `usize::MAX` and the process hung with no diagnostic. The Range
# itself was always correct -- `(0 .. -1).elems` is 0 -- it was the subscript
# that never noticed the range was empty.
#
# A negative START is a different case: raku throws X::OutOfRange there, exactly
# as it does for a plain `@a[$neg]` read, rather than clamping to 0.

plan 23;

# The reported hang, and the empty answer raku gives.
{
    my @el = (4,);
    my $e = -1;
    is-deeply @el[0 .. $e], (), 'array slice with a runtime negative end is empty';
}

{
    my @a = 1, 2, 3;
    my $e = -1;
    is @a[0 .. $e].elems, 0, 'runtime negative end yields no elements';
    my $far = -3;
    is-deeply @a[0 .. $far], (), 'a more negative end is still empty';
    my $one = 0;
    is-deeply @a[0 .. $one], (1,), 'a zero end still addresses index 0';
}

# The same window logic backs List/Seq/Range targets, so pin them too.
{
    my $e = -1;
    is-deeply (1, 2, 3).List[0 .. $e], (), 'List slice with a runtime negative end is empty';
    is-deeply (1, 2, 3).Seq[0 .. $e], (), 'Seq slice with a runtime negative end is empty';
    is-deeply [1, 2, 3][0 .. $e], (), 'itemized array slice with a runtime negative end is empty';
    is-deeply "abcd".comb[0 .. $e], (), 'comb slice with a runtime negative end is empty';
    is-deeply (10 .. 20)[0 .. $e], (), 'Range slice with a runtime negative end is empty';
}

# The associative twin shares the subscript entry point.
{
    my %h = a => 1;
    my $e = -1;
    is-deeply %h{0 .. $e}, (), 'hash slice with a runtime negative end is empty';
}

# A negative START throws, matching raku.
{
    my @a = 1, 2, 3;
    my $s = -1;
    throws-like { @a[$s .. 2] }, X::OutOfRange,
        'a runtime negative slice start throws X::OutOfRange';
    throws-like { @a[$s .. $s] }, X::OutOfRange,
        'a wholly negative runtime slice range throws X::OutOfRange';
    throws-like { (1, 2, 3).Seq[$s .. 2] }, X::OutOfRange,
        'a runtime negative Seq slice start throws X::OutOfRange';
    throws-like { (10 .. 20)[$s .. 2] }, X::OutOfRange,
        'a runtime negative Range slice start throws X::OutOfRange';
}

# Controls: nothing about the ordinary slice shapes changed.
{
    my @a = 1, 2, 3;
    is-deeply @a[0 .. 2], (1, 2, 3), 'a full inclusive slice is unchanged';
    is-deeply @a[1 .. *], (2, 3), 'an unbounded end still clips at the boundary';
    is-deeply @a[*-2 .. *-1], (2, 3), 'a WhateverCode range still counts from the end';
    my $big = 5;
    is-deeply @a[0 .. $big], (1, 2, 3, Any, Any, Any),
        'an over-long end still pads with the array default';
    is-deeply @a[0 ..^ 2], (1, 2), 'the exclusive-end twin is unchanged';
}

{
    my @empty;
    is-deeply @empty[0 .. *], (), 'an unbounded slice of an empty array is empty';
    is-deeply @empty[0 .. 2], (Any, Any, Any),
        'a bounded slice of an empty array still pads with Any';
}

# The literal spelling stays a hard error, the way raku rejects it at compile time.
{
    throws-like 'my @el = (4,); @el[0 .. -1]', Exception,
        'a literal negative subscript is still rejected';
}

# The hang guard: run the repro in a child process with a wall-clock bound, so a
# regression fails the test instead of wedging the whole suite.
doesn't-hang 'my @el = (4,); my $e = -1; say @el[0 .. $e].elems;',
    'a runtime negative slice end terminates', :out("0\n");
