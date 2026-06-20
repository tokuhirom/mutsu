use Test;

# Slice F (env<->locals coherence) pin: a captured-outer lexical mutated by a
# *nested* callee (caller -> mid -> writer) must ACCUMULATE correctly across
# repeated calls, not just propagate a single write.
#
# The single-frame write-through slices (#3317 etc.) drained the captured-outer
# write at the directly-enclosing call site. For a multi-frame chain that drain
# happens one frame too deep (the intermediate `mid` frame has no slot for the
# variable), so the top-level slot stayed stale across calls and `via(); via()`
# failed to accumulate (10, not 20) once the reverse `sync_locals_from_env` pull
# was disabled. The fix reconciles the caller's slots from env at the cached
# fast-call / closure / wrap / carrier dispatch sites, env_dirty-gated exactly
# like the slow path. These assertions must hold identically with reverse-sync
# ON and OFF.

plan 10;

# 1. Named-sub chain: caller -> via -> bump-outer writes $acc.
{
    my $acc = 0;
    sub bump-outer() { $acc = $acc + 10 }
    sub via() { bump-outer() }
    via();
    is $acc, 10, 'single nested-sub call propagates';
    via();
    is $acc, 20, 'repeated nested-sub calls accumulate';
}

# 2. Post-increment through a nested sub.
{
    my $cnt = 0;
    sub inc() { $cnt++ }
    sub mid() { inc() }
    mid(); mid(); mid();
    is $cnt, 3, 'nested ++ accumulates across calls';
}

# 3. Closure forwarder: $apply is not a leaf; $sum is not its free var.
{
    my $sum = 0;
    my $add = -> $n { $sum = $sum + $n };
    my $apply = -> $blk, $v { $blk($v) };
    $apply($add, 5);
    is $sum, 5, 'single closure-forwarded mutation propagates';
    $apply($add, 7);
    is $sum, 12, 'repeated closure-forwarded mutations accumulate';
}

# 4. Positional-arg sub chain (positional-light dispatch).
{
    my $total = 0;
    sub addn($n) { $total = $total + $n }
    sub relay($n) { addn($n) }
    relay(3); relay(4);
    is $total, 7, 'nested positional-sub calls accumulate';
}

# 5. Mixed: a captured array push through a nested sub.
{
    my @log;
    sub record($x) { @log.push($x) }
    sub forward($x) { record($x) }
    forward('a'); forward('b');
    is @log.join(','), 'a,b', 'nested captured-array push accumulates';
}

# 6. String concatenation accumulation through a nested sub.
{
    my $s = "";
    sub app($c) { $s = $s ~ $c }
    sub go($c) { app($c) }
    go('x'); go('y'); go('z');
    is $s, 'xyz', 'nested string accumulation';
}

# 7. Carrier chain: an `is raw` slurpy writeback to a caller scalar that runs
# *inside* an interpreter routine (`lives-ok { ... }`) must reach the carrier
# caller's slot, not just env.
{
    my @arr = 1..5;
    my $scal = 42;
    lives-ok {
        my sub mutate(*@list is raw) { @list[*-1] = 'ho' }
        mutate(@arr, $scal);
    }, 'slurpy raw inside carrier lives';
    is $scal, 'ho', 'slurpy raw writeback through carrier reaches caller slot';
}
