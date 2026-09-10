use Test;
plan 3;

# `Supply.do($cb)` on an on-demand (`supply { ... }`) source used to be a
# permanent dead end: the fallback that builds the derived Supply copied only
# `values` (empty for an on-demand source, which never materializes ahead of
# time) and `live`, dropping `on_demand_callback` entirely. A later `.tap`/
# `whenever` on the result then had nothing to run — the do callback never
# fired and no value ever reached the tap.
#
# This is what hung the vendored Cro suite's `http-auth-basic.rakutest`:
# `Cro::HTTP::Auth::Basic.process-responses` is `$responses.do: -> $r { ... }`
# where `$responses` is an on-demand supply, and the `whenever` consuming its
# result never fired, so no HTTP response ever reached the client.

{
    my @seen;
    my $s = supply { emit 1; emit 2; emit 3 };
    my @tapped;
    $s.do({ @seen.push($_) }).tap({ @tapped.push($_) });
    is @seen, (1, 2, 3), 'the do callback fires for each value from an on-demand source';
    is @tapped, (1, 2, 3), 'the derived Supply still delivers values to its own tap';
}

# `.do` still works when chained after another `.do` on an on-demand source.
{
    my @first;
    my @second;
    my $s = supply { emit 'a'; emit 'b' };
    $s.do({ @first.push($_) }).do({ @second.push($_) }).tap(-> $ { });
    is (@first, @second), (('a', 'b'), ('a', 'b')), 'chained .do calls on an on-demand source both fire';
}
