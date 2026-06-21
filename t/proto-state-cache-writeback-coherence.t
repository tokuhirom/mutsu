use Test;

# A caching `proto` whose `{*}` redispatch is evaluated as the RHS of a
# `state %`-hash element assignment must accumulate the cache across calls.
#
# Root cause this pins (Sub-slice 1b): the `{*}` redispatch swaps `self.env`
# out from under the proto body's local slot (restore_env_preserving_existing),
# diverging the env hash Arc from the local slot. The fast hash element-assign
# then mutated only env (strong_count == 1), leaving the local slot stale; a
# later `sync_env_from_locals` flushed the stale slot back over env, so the
# `state`-var persist saved an empty hash and the cache was lost across calls.
# Verified ON (blanket reconcile) and OFF (MUTSU_NO_BLANKET_RECONCILE, the
# single-store path) both pass.

plan 6;

{
    my $called_with = '';
    proto cached($a) {
        state %cache;
        %cache{$a} //= {*}
    }
    multi cached($a) {
        $called_with ~= $a;
        $a x 2;
    }
    is cached('a'), 'aa', 'caching proto (1)';
    is cached('b'), 'bb', 'caching proto (2)';
    is cached('a'), 'aa', 'caching proto (3) reuses the cached value';
    is $called_with, 'ab', 'cached value did not cause an extra multi call';
}

# Explicit element-assign (no `//=`) with a `{*}` RHS accumulates too.
{
    my $calls = 0;
    proto pick($a) {
        state %seen;
        %seen{$a} = {*} unless %seen{$a}:exists;
        %seen{$a};
    }
    multi pick($a) { $calls++; $a ~ $a }
    pick('x'); pick('y'); pick('x'); pick('y'); pick('x');
    is $calls, 2, 'each distinct key dispatched to the multi exactly once';
}

# A plain `state @`-array push inside a proto body still persists (regression
# guard for the divergence-repair change).
{
    my @log;
    proto collect($x) {
        state @buf;
        @buf.push($x);
        @log = @buf.clone;
        {*}
    }
    multi collect($x) { $x }
    collect(1); collect(2); collect(3);
    is @log.join(','), '1,2,3', 'state array accumulates across proto calls';
}
