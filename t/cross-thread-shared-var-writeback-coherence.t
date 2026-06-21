use Test;

# Slice 1b (env<->locals coherence): a cross-thread update (a worker `start`
# block mutating a captured outer variable, observed after `await`/`.result`)
# lands in the parent env via `sync_shared_vars_to_env`. The parent's matching
# *local slot* must be reconciled from env via the precise
# `pending_rw_writeback_sources` drain — independent of the blanket reverse-sync
# pull. Each subtest must hold both with the blanket reconcile ON (default) and
# OFF (`MUTSU_NO_BLANKET_RECONCILE=1`). These are deterministic (single worker,
# joined before the read), not timing-dependent.

plan 6;

# 1. cas on an array captured by an awaited worker promise.
{
    my $seen = [];
    Promise.allof(start { cas $seen, -> @current { flat @current, 1 } }).result;
    is ~$seen, '1', 'allof.result syncs cas array update to caller slot';
}

# 2. Two cas appends across two workers, both awaited.
{
    my $acc = [];
    Promise.allof(
        (start { cas $acc, -> @c { flat @c, 1 } }),
    ).result;
    Promise.allof(
        (start { cas $acc, -> @c { flat @c, 2 } }),
    ).result;
    is ~$acc, '1 2', 'sequential awaited cas updates accumulate in caller slot';
}

# 3. scalar atomic increment via cas, awaited.
{
    my $n = 0;
    Promise.allof(start { cas $n, -> $v { $v + 1 } }).result;
    is $n, 1, 'awaited cas scalar increment visible in caller slot';
}

# 4. caller slot coherent in a later expression.
{
    my $seen = [];
    Promise.allof(start { cas $seen, -> @c { flat @c, 7 } }).result;
    my $doubled = $seen[0] * 2;
    is $doubled, 14, 'caller slot coherent in a later expression';
}

# 5. await on a single start promise that cas-updates a captured hash.
{
    my $h = {};
    Promise.allof(start { cas $h, -> %c { %c<k> = 1; %c } }).result;
    is $h<k>, 1, 'awaited cas hash update visible in caller slot';
}

# 6. unrelated intervening read does not lose the cross-thread update.
{
    my $seen = [];
    Promise.allof(start { cas $seen, -> @c { flat @c, 9 } }).result;
    my $unrelated = 1 + 1;
    is ~$seen, '9', 'cross-thread update survives an intervening expression';
}
