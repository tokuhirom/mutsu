use Test;

# Regression: `(1..Inf).Supply` used to `capacity overflow`-panic because the
# Supply coercion expanded the infinite Range to a Vec eagerly (ANALYSIS §8.7).
# It must now coerce without crashing (capped at MAX_RANGE_EXPAND), and a `done`
# inside the tap callback must terminate the tap cleanly rather than surfacing as
# a runtime error.

plan 4;

{
    my $s = (1..Inf).Supply;
    isa-ok $s, Supply, "(1..Inf).Supply coerces without crashing";
}

{
    my @seen;
    (1..Inf).Supply.tap({ @seen.push($_); done if $_ >= 3 });
    is-deeply @seen, [1, 2, 3], "done inside tap stops an infinite-range Supply";
}

{
    my @seen;
    (1..5).Supply.tap({ @seen.push($_) });
    is-deeply @seen, [1, 2, 3, 4, 5], "finite-range Supply still replays all values";
}

{
    my @seen;
    (1..Inf).Supply.tap({ @seen.push($_); last if @seen == 2 });
    is-deeply @seen, [1, 2], "last inside tap also stops the Supply cleanly";
}
