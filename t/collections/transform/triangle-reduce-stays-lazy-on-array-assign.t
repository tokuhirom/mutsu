use Test;

# `my @n = [\~] 1..*` hung. The triangle reduce over a lazy source produces a
# `LazyList`, but `LazyList::preserve_lazy_on_array_assign` did not recognise a
# scan, so the `@`-assignment forced it -- 200_000 elements. For `[\+]` that was
# merely slow (the test below would have taken ~235ms instead of ~23ms); for
# `[\~]`, where each element is one character longer than the last, the forcing
# is quadratic in bytes and never finished.
#
# `is_genuinely_lazy` already carried the `scan_spec` arm unconditionally, so
# the two predicates simply disagreed. A scan only becomes a `LazyList` when its
# input is already lazy, so no finiteness test is needed -- and a finite
# `[\+] 1..10` must still materialize, which is asserted below.
#
# Every expectation was measured against raku v2026.07 first.

plan 14;

{
    my @n = [\~] 1..*;
    is-deeply @n[^5].List, (1, "12", "123", "1234", "12345"), 'the doc example';
    ok @n.is-lazy, '... and the array stays lazy';
}

# The other operators over the same infinite source.
{
    my @n = [\+] 1..*;
    is-deeply @n[^5].List, (1, 3, 6, 10, 15), '[\+] over an infinite range';
    ok @n.is-lazy, '... stays lazy too';
}
{
    my @n = [\*] 1..*;
    is-deeply @n[^5].List, (1, 2, 6, 24, 120), '[\*] over an infinite range';
    ok @n.is-lazy, '... stays lazy too';
}
{
    my @n = [\~] 1..*;
    ok @n.is-lazy, 'a second infinite scan is lazy';
    is-deeply @n.head(5).List, (1, "12", "123", "1234", "12345"), '.head reads a prefix';
}

# A FINITE scan must still materialize -- it never becomes a LazyList at all.
{
    my @n = [\+] 1..10;
    nok @n.is-lazy, 'a finite scan is not lazy';
    is @n.elems, 10, '... and knows its length';
    is-deeply @n.List, (1, 3, 6, 10, 15, 21, 28, 36, 45, 55), '... with the right elements';
}
{
    my @n = [\~] <a b c>;
    nok @n.is-lazy, 'a finite scan over a list is not lazy';
    is-deeply @n.List, ('a', 'ab', 'abc'), '... with the right elements';
}

# The spellings that already worked must keep working.
{
    my $n = [\~] 1..*;
    is-deeply $n[^5].List, (1, "12", "123", "1234", "12345"), 'assignment into a $ scalar';
}
