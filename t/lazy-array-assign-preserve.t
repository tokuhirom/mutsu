use v6;
use Test;

# L2b step 6 (docs/lazy-arrays.md): a genuinely-lazy list assigned to an
# `@` array survives as a reify-on-demand lazy array, matching raku.
# Found by the doc-diff sweep (Language/list.rakudoc [7], [1]).

plan 17;

# Infinite sequence assigned directly.
{
    my @a = 1, 2, 4, 8 ... Inf;
    ok @a.is-lazy, 'infinite ... sequence array is lazy';
    is @a[3], 8, 'indexing reifies on demand';
    is-deeply @a[10..12], (1024, 2048, 4096), 'slice past the seed reifies';
    is @a.gist, '[...]', 'gist renders the lazy placeholder';
}

# Infinite sequence through a bound scalar.
{
    my $l := 1, 2, 4, 8 ... Inf;
    my @lazy-array = $l;
    ok @lazy-array.is-lazy, 'sequence via bound scalar stays lazy';
    is-deeply @lazy-array[10..15], (1024, 2048, 4096, 8192, 16384, 32768),
        'doc example slice matches';
}

# The `lazy` prefix takes the whole sequence as its operand (looser than
# comma), so the marker survives onto the sequence itself.
{
    my $s = lazy 1, 11, 121 ... 10**6;
    ok $s.is-lazy, 'lazy prefix on a ... sequence marks the sequence';
    my @lazy-array = lazy 1, 11, 121 ... 10**6;
    ok @lazy-array.is-lazy, 'and it survives array assignment';
    is @lazy-array[3], 1331, 'elements still reify';
}

# Mutation semantics: bounded partial reify, and the array genuinely stays
# lazy afterward (raku reifies only up to the touched index -- it does NOT
# collapse the rest of an infinite source to a finite Array).
{
    my @a = 1, 2, 4 ... Inf;
    @a[2] = 99;
    ok @a.is-lazy, 'element assign does not collapse the array to a finite one';
    is-deeply @a[^4], (1, 2, 99, 8), 'element assign reifies a prefix, tail stays live';
    is @a[10], 1024, 'a later out-of-range read still reifies further from the live source';
    throws-like { @a.elems }, X::Cannot::Lazy,
        '.elems still throws after mutation -- the tail is genuinely still lazy';
}
{
    my @a = 1, 2, 4 ... Inf;
    @a[2]:delete;
    ok @a.is-lazy, ':delete does not collapse the array to a finite one';
    is-deeply @a[^4], (1, 2, Any, 8),
        ':delete leaves a hole at the touched index, tail stays live';
}
{
    my @a = 1, 2, 4 ... Inf;
    throws-like { @a.push(9) }, X::Cannot::Lazy, 'push on a lazy array throws';
}

# A plain finite gather still materializes eagerly (is-lazy False).
{
    my @a = gather { take 1; take 2 };
    nok @a.is-lazy, 'plain gather array is not lazy';
}

done-testing;
