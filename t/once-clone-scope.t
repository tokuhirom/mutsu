use Test;

# Regression pins for `once` clone-scope keying:
#  * a named sub's `once` fires once no matter how many times it is called
#    (the second call used to take a light call path that skipped the clone-id
#    setup, giving it a different once key -> a spurious second fire);
#  * a `once` in a sub run from several `start` blocks fires once across threads
#    (the per-thread `once_values` copy + a non-deterministic compile-time key
#    made every worker re-fire);
#  * a fresh `my sub` clone per loop iteration re-fires (clone identity, not
#    just the source site, drives the key).

plan 5;

# 1. Named sub called many times: exactly one fire.
{
    my $n = 0;
    sub f() { once { $n++ } }
    f(); f(); f(); f();
    is $n, 1, 'once in a named sub fires once across many calls';
}

# 2. Two distinct named subs keep independent once state.
{
    my @log;
    sub a() { once { @log.push: 'a' } }
    sub b() { once { @log.push: 'b' } }
    a(); a(); b(); b();
    is-deeply @log, ['a', 'b'], 'distinct named subs have independent once state';
}

# 3. A fresh `my sub` clone per loop iteration re-fires (once per clone).
{
    my $c = 0;
    for ^3 {
        my sub inner() { once { $c++ } }
        inner(); inner();
    }
    is $c, 3, 'a fresh my-sub clone per iteration re-fires once each';
}

# 4. once in a sub run from several start blocks: one fire across threads.
{
    my $count = 0;
    sub worker() { once { $count++ } }
    my @p = (^8).map: { start { worker() } };
    await @p;
    is $count, 1, 'once in a sub fires once across threads';
}

# 5. once still returns (and caches) its value as an rvalue in a sub.
{
    sub v() { my $x = once { 7 }; $x }
    is v() + v(), 14, 'once rvalue value is cached and reused';
}
