use Test;

plan 22;

# An endpoint-less closure sequence is lazy because of its `... *` iterator,
# not because it never ends: a generator that ends it with `last` inside the
# first few elements still yields a lazy Seq, and assigning it to an
# `@`-array keeps the array lazy. Only a strict force (`.eager`) answers the
# complete list (#11098).

sub ending() { 1, { last if $_ >= 5; $_ + 1 } ... * }

ok ending().is-lazy, 'the Seq is lazy';
throws-like { ending().elems }, X::Cannot::Lazy, '.elems on the Seq throws';
throws-like { ending().sum }, X::Cannot::Lazy, '.sum on the Seq throws';
is-deeply ending().eager.List, (1, 2, 3, 4, 5), '.eager forces it completely';
is-deeply ending().head(3).List, (1, 2, 3), '.head reads a prefix';
is-deeply ending()[^7], (1, 2, 3, 4, 5, Nil, Nil), 'a Seq slice past the end is Nil';
is ending().gist, '(...)', '.gist is the lazy placeholder';
ok ending().map(* * 2).is-lazy, 'a map over it is lazy too';
{
    my @seen;
    for ending() { @seen.push($_) }
    is-deeply @seen, [1, 2, 3, 4, 5], 'for iterates every element';
}

{
    my @a = 1, { last if $_ >= 5; $_ + 1 } ... *;
    ok @a.is-lazy, 'the @-array is lazy';
    throws-like { @a.elems }, X::Cannot::Lazy, '.elems on the array throws';
    throws-like { @a[*-1] }, X::Cannot::Lazy, 'a *-1 index throws';
    throws-like { @a.push(9) }, X::Cannot::Lazy, '.push throws';
    is @a.raku, '[...]', '.raku is the lazy placeholder';
    is-deeply @a[^7], (1, 2, 3, 4, 5, Any, Any), 'an array slice past the end is Any';
    is-deeply @a.eager, [1, 2, 3, 4, 5], '.eager forces the array';
}
{
    my @b = 1, 2, { last if $_ >= 5; $_ + 1 } ... *;
    ok @b.is-lazy, 'two seeds: still lazy';
    @b.shift;
    is-deeply @b[^5], (2, 3, 4, 5, Any), 'shift keeps reading the ended sequence';
}

# The same reads on any lazy @-array: a slot past the reified end is the
# element default Any, while a bare lazy list answers Nil.
{
    my @l = lazy 1, 2;
    is-deeply @l[^4], (1, 2, Any, Any), 'lazy @-array of a list';
    my @g = lazy gather { take 1; take 2 };
    is-deeply @g[3], Any, 'lazy @-array of a gather';
    my $s = lazy 1, 2;
    is-deeply $s[^4], (1, 2, Nil, Nil), 'a bare lazy list stays Nil';
}

# A closure sequence that never ends is unchanged.
{
    my @n = 1, { $_ + 1 } ... *;
    ok @n.is-lazy, 'an endless closure sequence is still lazy';
}
