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

plan 10;

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

# 6. once in a method fires once per clone (once total for a normal method,
#    across instances and repeated calls) — the clone key is (class, method),
#    not the method's per-call callable id.
{
    my $n = 0;
    my class C { method m() { once { $n++ } } }
    my $a = C.new; my $b = C.new;
    $a.m; $a.m; $b.m; $b.m;
    is $n, 1, 'once in a method fires once across instances and calls';
}

# 7. A role method fires once per composing class (a distinct clone each).
{
    my @log;
    my role Loud { method shout() { once { @log.push: self.^name } } }
    my class D does Loud {}
    my class E does Loud {}
    my $d = D.new; my $e = E.new;
    $d.shout; $d.shout; $e.shout; $e.shout;
    is-deeply @log.sort.List, ('D', 'E'), 'role method once fires once per composing class';
}

# 8. Two once-bearing methods in one class keep independent state.
{
    my @log;
    my class F { method a() { once { @log.push: 'a' } }; method b() { once { @log.push: 'b' } } }
    my $f = F.new;
    $f.a; $f.a; $f.b; $f.b;
    is-deeply @log, ['a', 'b'], 'distinct methods have independent once state';
}

# 9. A closure created per-iteration inside a once-method keeps its own per-clone
#    once state (the method's clone key must not leak into the nested closure).
{
    my $method-fires = 0;
    my $closure-fires = 0;
    my class G {
        method go() {
            once { $method-fires++ }
            for ^3 { my &c = { once { $closure-fires++ } }; c(); c(); }
        }
    }
    my $g = G.new;
    $g.go; $g.go;
    is $method-fires, 1, 'method once fires once even when it wraps nested closures';
    is $closure-fires, 6, 'nested per-iteration closure once fires once per clone';
}
