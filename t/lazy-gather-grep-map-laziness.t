use Test;

# A `.map`/`.grep` over a gather must stay lazy: pulling only the elements a
# downstream slice needs, never forcing the whole gather body (and its trailing
# side effects). Previously the gather was eager-forced to a Seq before the
# method ran, which executed the tail statement of the gather body. This is the
# laziness substrate behind roast S02-types/lazy-lists.t subtests 14/16
# (`grep is lazy` / `map is lazy`).

plan 8;

# grep is lazy: gather tail never runs when only a prefix is consumed.
{
    my $was-lazy = 1;
    sub make-lazy-grep { gather { take $_ for 0..^10; $was-lazy = 0 }.lazy };
    is make-lazy-grep.grep(*.is-prime)[^3], (2, 3, 5), "grep yields first three primes";
    ok $was-lazy, "grep is lazy (gather tail not run)";
}

# map is lazy: same, via a map stage.
{
    my $was-lazy = 1;
    sub make-lazy-map { gather { take $_ for 0..^10; $was-lazy = 0 }.lazy };
    is make-lazy-map.map({ $^n * 2 })[^3], (0, 2, 4), "map yields first three doubled";
    ok $was-lazy, "map is lazy (gather tail not run)";
}

# A plain gather (no `.lazy`) is also lazy under grep+slice in Rakudo.
{
    my $tail = 0;
    sub plain { gather { take $_ for 0..^10; $tail = 1 } };
    is plain.grep(*.is-prime)[^3], (2, 3, 5), "plain gather grep yields primes";
    ok !$tail, "plain gather grep is lazy too";
}

# Full consumption still yields every matching element (the lazy pipe pulls the
# whole gather to exhaustion when the result is fully consumed).
{
    my @a = gather { take 1; take 2; take 3 }.grep(* > 1);
    is @a, (2, 3), "full grep consumption yields all matching elements";
}

# Chained lazy grep + map keeps laziness across both stages.
{
    my $tail = 0;
    sub chained { gather { take $_ for 1..100; $tail = 1 }.lazy };
    is chained.grep(* %% 2).map(* * 10)[^3], (20, 40, 60), "chained grep+map slice";
}
