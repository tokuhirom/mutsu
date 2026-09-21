use v6;
use Test;

# `.m()` on a NAMED VARIABLE (`my $o = C.new; $o.m()`) always compiles to
# CallMethodMut, not the plain CallMethod opcode. Before #8880's fix,
# `try_compiled_method_mut_or_interpret_sym` never consulted or populated
# `fast_method_cache`, so every such call re-ran the full
# `resolve_method_cached` -> `check_method_wrap_chain` -> `attributes.to_map()`
# walk from scratch -- even the second+ call in a tight loop on the same
# (class, method) pair. This file exercises the fast-cache HIT path this
# opcode now takes, and the cases that must still bypass it: a wrapped
# method (must see every wrap/unwrap), a multi method (never cached at all),
# and attribute state that changes between calls (the fast path must read
# live state, not a stale snapshot).

plan 7;

# A plain repeated call on a named-variable receiver: first call populates
# the cache, every later call should hit it and still return correct,
# per-instance results.
{
    class Counter { has $.n = 0; method bump() { $!n++; $!n } }
    my $c = Counter.new;
    my @seen = (1..5).map({ $c.bump });
    is @seen.join(","), "1,2,3,4,5",
        "repeated CallMethodMut calls on the same variable stay correct across cache hits";
}

# Two instances of the same class, called alternately through the SAME
# named-variable receiver (rebound each iteration) must never observe the
# other instance's state -- the fast path must key strictly off the resolved
# method, not memoize a snapshot of one instance's attributes.
{
    class Box { has $.v; method get() { $!v } }
    my $a = Box.new(v => "A");
    my $b = Box.new(v => "B");
    my $o = $a;
    my @out;
    for 1..4 {
        $o = ($_ % 2 == 0) ?? $b !! $a;
        @out.push($o.get);
    }
    is @out.join(","), "A,B,A,B",
        "alternating receivers through one variable read live per-instance state";
}

# A wrapped method reached through a named-variable receiver: the cache must
# be invalidated by `.wrap`/unwrap (method_generation bump), never serving a
# stale pre-wrap resolution.
{
    my @ev;
    class W { method m() { @ev.push("orig"); "orig" } }
    my $w = W.new;
    is $w.m, "orig", "unwrapped call (cache miss) returns the plain method";
    is $w.m, "orig", "unwrapped call (cache hit) still returns the plain method";
    my $h = W.^lookup('m').wrap(-> |c { @ev.push("wrapped"); my $r = callsame; "w-$r" });
    is $w.m, "w-orig", "wrapping after the cache is warm is still observed";
    W.^lookup('m').unwrap($h);
    is $w.m, "orig", "unwrapping after the cache is warm restores the original";
}

# A multi method dispatched through a named-variable receiver must keep
# choosing the candidate matching each call's arguments -- it must never be
# cached (fast_method_cache excludes multis by construction), so alternating
# argument types across calls must not "stick" to whichever candidate ran
# first.
{
    class M {
        multi method describe(Int $x) { "int:$x" }
        multi method describe(Str $x) { "str:$x" }
    }
    my $m = M.new;
    my @out = (1, "a", 2, "b").map({ $m.describe($_) });
    is @out.join(","), "int:1,str:a,int:2,str:b",
        "multi method via a named-variable receiver keeps resolving per call";
}
