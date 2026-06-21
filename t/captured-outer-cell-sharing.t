use Test;

# Sub-slice 1b (env<->locals cell sharing) pin: a scalar lexical captured AND
# mutated by a directly-nested *named sub* is boxed into a shared ContainerRef
# cell at its declaration site, so the named sub's by-name env writes and the
# owner's reads observe one cell. This makes cross-call accumulation correct
# WITHOUT the `env_dirty` blanket reconcile (docs/captured-outer-cell-sharing.md).
#
# These assertions must hold identically with the blanket reconcile ON and OFF
# (MUTSU_NO_BLANKET_RECONCILE=1) — the OFF run is the proof that the shared cell,
# not the reconcile, is carrying the coherence.

plan 12;

# 1-2. Nested named-sub chain accumulates (the via(); via() case), block-scoped
# so $acc is a true local slot.
{
    my $acc = 0;
    sub bump-outer() { $acc = $acc + 10 }
    sub via() { bump-outer() }
    via();
    is $acc, 10, 'single nested-sub call propagates through cell';
    via();
    is $acc, 20, 'repeated nested-sub calls accumulate through cell';
}

# 3. Direct named-sub post-increment accumulates.
{
    my $cnt = 0;
    sub tick() { $cnt++ }
    tick(); tick(); tick();
    is $cnt, 3, 'direct named-sub ++ accumulates';
}

# 4. += compound accumulation through a nested sub.
{
    my $sum = 100;
    sub addn($n) { $sum += $n }
    sub relay($n) { addn($n) }
    relay(1); relay(2); relay(3);
    is $sum, 106, 'nested += accumulates';
}

# 5. Captured local inside a *sub* frame (not top-level): outer() owns $x,
# inner() captures and mutates it.
{
    sub outer() {
        my $x = 0;
        sub inner() { $x = $x + 5 }
        inner();
        inner();
        $x
    }
    is outer(), 10, 'captured local inside a sub frame accumulates';
}

# 6. Recursion: the writer is reached through a recursive chain.
{
    sub f() {
        my $n = 0;
        sub bump() { $n++ }
        sub rec($d) { bump(); rec($d - 1) if $d > 0 }
        rec(3);
        $n
    }
    is f(), 4, 'captured var accumulates across a recursive chain';
}

# 7-8. Two distinct captured vars in the same scope, each gets its own cell.
{
    my $a = 1;
    my $b = 10;
    sub g() { $a += 2; $b += 20 }
    g();
    g();
    is $a, 5, 'first captured var cell accumulates';
    is $b, 50, 'second captured var cell accumulates';
}

# 9. Three-level dispatch chain.
{
    my $c = 0;
    sub w() { $c = $c + 1 }
    sub m1() { w() }
    sub m2() { m1() }
    m2(); m2(); m2();
    is $c, 3, 'three-level chain accumulates';
}

# 10. String accumulation through a nested sub (non-numeric value in the cell).
{
    my $s = "";
    sub app() { $s = $s ~ "x" }
    sub appvia() { app() }
    appvia();
    appvia();
    is $s, "xx", 'string captured var accumulates through cell';
}

# 11. The owner can also read the captured var by name between writes.
{
    my $v = 0;
    sub setv($n) { $v = $n }
    setv(7);
    is $v, 7, 'owner reads captured cell after a nested write';
}

# 12. A read-only capture (named sub only reads) must NOT corrupt the value.
{
    my $ro = 42;
    sub peek() { $ro }
    my $got = peek();
    is $got, 42, 'read-only captured var stays intact';
}
