use Test;

# A synchronous `.tap` callback over a Supply is an immediately-invoked closure
# run on this thread; its captured-outer writes (`$sum`, `$count`, `$min`/`$max`
# in roast S17-supply/throttle.t) must propagate back to the caller's lexicals.
# Previously the callback's writes only reached the env and were recovered by the
# blanket reconcile; under the env_dirty-removal simulation they were lost,
# leaving the captured scalars at their initial values. The `.tap` emission loop
# now records the callback's captured-outer writes for precise write-back.

plan 4;

# Scalar accumulation across tap emissions.
{
    my $sum = 0;
    my $count = 0;
    (1..5).Supply.tap: {
        $sum += $_;
        $count++;
    };
    is $sum, 15, "tap callback sum propagates to caller";
    is $count, 5, "tap callback count propagates to caller";
}

# min/max-style reductions (the shape exercised by throttle.t timing asserts).
{
    my $min = 1000;
    my $max = -1000;
    (3, 1, 4, 1, 5, 9, 2, 6).Supply.tap: {
        $min = $min min $_;
        $max = $max max $_;
    };
    is $min, 1, "tap callback min reduction propagates";
    is $max, 9, "tap callback max reduction propagates";
}
