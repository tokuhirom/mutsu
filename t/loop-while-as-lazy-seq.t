use v6;
use Test;

# `(loop { ... })` / `(while ...)` / `(until ...)` expressions are lazy Seqs
# pulled on demand (lowered to a gather-wrapped loop), so an infinite loop
# body only runs as many iterations as are consumed.
# Found by the doc-diff sweep (Language/list.rakudoc [4]/[6]/[9]/[12]).

plan 10;

# --- (loop { ... })[N] pulls exactly N+1 iterations ---
{
    my $runs = 0;
    my $v = (loop { $runs++; 42 })[2];
    is $v, 42, 'infinite loop expression indexes on demand';
    is $runs, 3, 'exactly three iterations ran for [2]';
}

# --- .list keeps the pull lazy, with caching across indexes ---
{
    my $runs = 0;
    my @s := (loop { $runs++; 42 }).list;
    @s[2];
    is $runs, 3, '@s[2] pulled three iterations';
    @s[1];
    is $runs, 3, '@s[1] answered from cache (no extra pulls)';
    @s[4];
    is $runs, 5, '@s[4] pulled two more';
}

# --- interleaving: map pulls one iteration at a time ---
{
    my @order;
    my $c = 0;
    (while $c++ < 2 { @order.push("body"); 43 }).map: { @order.push("map") };
    is @order.join(","), 'body,map,body,map',
        'while-expression Seq interleaves with its consumer';
}

# --- slice assignment pulls what the LHS needs ---
{
    my @a;
    @a[0, 1, 2] = (loop { 42 });
    is-deeply @a, [42, 42, 42], 'slice assignment from an infinite loop';
}

# --- do while / do loop still collect eagerly with correct values ---
{
    my $i = 0;
    my @r = do while $i < 3 { $i++; $i * 10 };
    is-deeply @r, [10, 20, 30], 'do while collects iteration values';
    is $i, 3, 'do while side effects applied on array assignment';
    my @c = do loop (my $j = 0; $j < 3; $j++) { $j * 2 };
    is-deeply @c, [0, 2, 4], 'do loop collects iteration values';
}

done-testing;
