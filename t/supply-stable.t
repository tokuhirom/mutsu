use Test;

# Supply.stable($time): debounce transformer.
# - `.stable(0)` is a no-op and returns the *same* Supply (=== identical).
# - Calling `stable` on the type object dies.

plan 4;

dies-ok { Supply.stable(1) }, 'stable cannot be called as a class method';

{
    my $for    = Supply.from-list(1..10);
    my $stable = $for.stable(0);
    ok $for === $stable, 'stable by 0 is a no-op (identical supply)';

    my @got;
    $stable.tap({ @got.push($_) });
    is-deeply @got, [1..10], 'no-op stable replays all values';
}

{
    # For an instantaneously-emitting (materialized) supply, a positive stable
    # window collapses to just the final value.
    my @got;
    Supply.from-list(1..5).stable(2).tap({ @got.push($_) });
    is-deeply @got, [5], 'instant emission debounces to the last value';
}
