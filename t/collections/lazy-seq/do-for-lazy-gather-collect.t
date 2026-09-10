use Test;

# The expression form `my @a = do for <lazy source> { ... }` must collect
# EVERY iteration's block value. The lazy-gather for-loop had no collect
# protocol at all, so iteration values piled up on the VM stack and only the
# top one reached the assignment ("the first value is dropped" from the
# caller's view — todo/tickets/do-for-over-lazy-gather-drops-first-value.md).
# Expected values verified against raku.

plan 5;

sub trip($n) { for 1..3 -> \a { take a * $n } }

{
    my @a = do for gather trip(5) { $_ };
    is-deeply @a, [5, 10, 15], 'do for over lazy gather collects every value';
}

{
    my @b = do for gather trip(2) { next if $_ == 4; $_ * 10 };
    is-deeply @b, [20, 60], 'next contributes no value';
}

{
    my @c = do for gather trip(3) { last if $_ == 6; $_ };
    is-deeply @c, [3], 'last stops the collection';
}

{
    my @d = do for (1, 2, * + 1 ... *) { last if $_ > 4; $_ * 2 };
    is-deeply @d, [2, 4, 6, 8], 'infinite closure sequence collects until last';
}

{
    my @got;
    for gather trip(7) { @got.push($_) }
    is-deeply @got, [7, 14, 21], 'statement form still iterates every element';
}
