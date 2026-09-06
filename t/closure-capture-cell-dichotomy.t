use v6;
use Test;

# ADR-0055 slice 1 -- "a closure's free variable resolves to its own captured
# binding", delivered by the shared container cell rather than by merge order.
#
# The invariant this file pins: EVERY escaping-captured plain scalar is either
# *authoritative* (the creating frame proves it never changes after capture, so
# a by-value snapshot is exact) or a shared `ContainerRef` cell. The two sets
# are complements by construction -- `CompiledCode::needs_cell_unvouched_locals`
# is literally the vouch's complement within the escaping-captured set.
#
# When a capture falls into neither, both failure directions appear:
#   * HIJACK    -- a same-named lexical in whatever frame happens to be calling
#                  shadows the closure's own binding (lexical scoping degrading
#                  into dynamic scoping), and
#   * STALENESS -- the creator's post-capture mutation is invisible.
# A cell fixes both at once; merge-order tweaks can only ever fix one.

plan 17;

# ---------------------------------------------------------------------------
# 1-3. The three value-kind families ADR-0025 slice 3 left unboxed, in the
# HIJACK direction. `collide`'s `my $g = { ... }` is load-bearing: it forces the
# caller's same-named lexical to be materialised into `env` rather than living
# only in a local slot, which is what exposes the merge to it at all.
# ---------------------------------------------------------------------------

# (a) a type-constrained scalar (a class type, not one of the value types)
{
    class Foo { has $.v }
    my Foo $x = Foo.new(v => 1);
    my $f = { $x.v };
    $x = Foo.new(v => 42);
    sub collide-a() { my $x = "DECOY"; my $g = { $x }; $g.(); $f.() }
    is collide-a(), 42,
        'type-constrained captured scalar keeps its own binding under a same-named caller lexical';
}

# (b) an Array held in a `$` scalar
{
    my $a = [1, 2];
    my $f = { $a.elems };
    $a = [1, 2, 3];
    sub collide-b() { my $a = "DECOY"; my $g = { $a }; $g.(); $f.() }
    is collide-b(), 3,
        '$-held Array capture keeps its own binding under a same-named caller lexical';
}

# (c) a Package (type object) held in a scalar
{
    class A { }
    class B { }
    my $p = A;
    my $f = { $p.^name };
    $p = B;
    sub collide-c() { my $p = "DECOY"; my $g = { $p }; $g.(); $f.() }
    is collide-c(), 'B',
        'Package-valued capture keeps its own binding under a same-named caller lexical';
}

# ---------------------------------------------------------------------------
# 4-6. The same three families in the STALENESS direction. These passed before
# the cell existed, but only by accident: the caller-priority merge read the
# live value out of the caller chain, which happened to contain the creating
# frame. They are pinned so a future merge change cannot silently trade the
# hijack fix for a staleness bug.
# ---------------------------------------------------------------------------
{
    my $a = [1, 2];
    my $fa = { $a.elems };
    $a = [1, 2, 3];
    is $fa(), 3, 'post-capture reassignment of a $-held Array is visible to the closure';

    class A2 { }
    class B2 { }
    my $p = A2;
    my $fp = { $p.^name };
    $p = B2;
    is $fp(), 'B2', 'post-capture reassignment of a Package-valued scalar is visible';

    class Foo2 { has $.v }
    my Foo2 $x = Foo2.new(v => 1);
    my $fx = { $x.v };
    $x = Foo2.new(v => 42);
    is $fx(), 42, 'post-capture reassignment of a type-constrained scalar is visible';
}

# ---------------------------------------------------------------------------
# 7. ADR-0055 section 1.2(b), slot-resident variant. A READ-ONLY capture of a
# lexical that was handed to a call gets NEITHER defence: `own_call_arg_sources`
# refuses to vouch for it (an `is rw` parameter could write it back), and it is
# never stored by name, so the mutation analysis never saw it either.
#
# Both residency variants are pinned: tests 12-15 add the env-resident form
# (`my $g = { $b }` in the caller forces the colliding lexical out of its local
# slot and into `env`), across four invocation paths. That form is what
# `needs_cell_unvouched_locals` closed; before it, the slot-resident variant
# passed only because a compiled caller usually keeps its lexicals in slots, so
# the merge's chain probe found nothing to collide with -- an accident of the
# `env_dirty` dual store, not a policy that was right.
# ---------------------------------------------------------------------------
{
    sub noop($v) { 1 }
    my $b = "OUTER";
    noop($b);                       # the vouch refusal
    my $f = { $b };
    sub collide-slot() { my $b = "CALLER"; $f.() }
    is collide-slot(), 'OUTER',
        'call-arg-sourced capture wins over a slot-resident same-named caller lexical';
}

# ---------------------------------------------------------------------------
# 8. The other half of the same refusal, in the staleness direction: the `is rw`
# writeback the vouch refusal exists to protect against must still be observed.
# ---------------------------------------------------------------------------
{
    sub mutate($x is rw) { $x = "NEW" }
    sub f-rw() {
        my $x = "orig";
        my $c = -> { $x };
        mutate($x);
        $c();
    }
    is f-rw(), 'NEW', 'an is-rw writeback through a call is visible to a capture made before it';
}

# ---------------------------------------------------------------------------
# 9. An in-place container write through a `$`-held Hash: the other vouch
# refusal (`own_container_writes`), invisible to the store-by-name signal.
# ---------------------------------------------------------------------------
{
    my $h = { a => 1 };
    my $f = { $h<a> };
    $h<a> = 9;
    is $f(), 9, 'an in-place write to a $-held Hash is visible to a capture made before it';
}

# ---------------------------------------------------------------------------
# 10-11. The bounds the boxing keeps.
#
# 10: an immediately-invoked block is deliberately NOT boxed -- it has no window
# in which the creator could mutate between capture and call, and that bound is
# what keeps the boxing cost off the common map/grep/sort path.
#
# 11: retiring the type-constraint refusal must not cost the constraint. The
# check belongs to the container (ADR-0042), so it re-runs on a write that
# reaches the scalar through its cell.
# ---------------------------------------------------------------------------
{
    my $n = 0;
    my @doubled = (1, 2, 3).map({ $_ + $n });
    $n = 10;
    is @doubled.join(','), '1,2,3', 'an immediately-invoked block reads the value live at call time';
}
{
    my Int $i = 1;
    my $c = { $i };
    $i = 7;
    my $err = 'no-error';
    try {
        $i = "not an Int";
        CATCH { default { $err = 'died' } }
    }
    is "{$c()}/$err", '7/died',
        'a now-boxed type-constrained scalar still type-checks its assignments';
}

# ---------------------------------------------------------------------------
# 12-15. ADR-0055 section 1.2(b), env-resident variant, across the four
# invocation paths that reach a closure. They differ in WHICH merge runs:
#   .()          -> `call_compiled_closure_with_topic` (the compiled merge)
#   .map($f)     -> `eval_map_over_items`' own inline pre-insert
#   .sort({...}) -> `call_sub_value` with merge_all: true (a native comparator)
#   invoke($f)   -> `call_sub_value` reached from compiled bytecode
# The capture is the same binding in all four, so all four must answer OUTER.
# ---------------------------------------------------------------------------
{
    sub noop2($v) { 1 }
    my $b = "OUTER";
    noop2($b);
    my $f = { $b };

    sub collide-env() { my $b = "CALLER"; my $g = { $b }; $g.(); $f.() }
    is collide-env(), 'OUTER',
        'call-arg-sourced capture wins over an env-resident same-named caller lexical';

    sub collide-map() { my $b = "CALLER"; my $g = { $b }; $g.(); (9,).map($f).join(',') }
    is collide-map(), 'OUTER', '... and through .map($f)';

    sub collide-sort() {
        my $b = "CALLER";
        my $g = { $b };
        $g.();
        (1, 2).sort({ $f.() cmp $f.() });
        $f.();
    }
    is collide-sort(), 'OUTER', '... and after a native comparator ran the closure';

    sub invoke(&c) { c() }
    sub collide-arg() { my $b = "CALLER"; my $g = { $b }; $g.(); invoke($f) }
    is collide-arg(), 'OUTER', '... and when the closure is invoked inside a callee';
}

# ---------------------------------------------------------------------------
# 16. The bound that keeps the cell from becoming a leak: a PARAMETER is a fresh
# binding the caller creates per invocation, so it must NOT be given a shared
# cell by this trigger. Boxing one made two invocations of the same routine
# share a binding -- every stored closure then read the last call's argument.
# (This is what dropped six Cro::HTTP suites when the mechanism was first
# prototyped; see the news entry.)
# ---------------------------------------------------------------------------
{
    sub noop3($v) { 1 }
    my @kept;
    sub mk($p) { noop3($p); @kept.push({ $p }); }
    mk("A"); mk("B"); mk("C");
    is @kept.map({ $_.() }).join(','), 'A,B,C',
        'a closure over a parameter keeps its own invocation\'s binding';
}

# ---------------------------------------------------------------------------
# 17. Two closures over the same name at different depths, both called from a
# frame that shadows it. Each must resolve to the binding it captured.
# ---------------------------------------------------------------------------
{
    sub noop4($v) { 1 }
    my $b = "OUTER";
    noop4($b);
    my $f1 = { $b };
    my $f2 = do { my $b = "MID"; noop4($b); my $inner = { $b }; $inner };
    sub collide-depths() { my $b = "CALLER"; my $g = { $b }; $g.(); $f1.() ~ '/' ~ $f2.() }
    is collide-depths(), 'OUTER/MID',
        'two captures of one name at different depths keep their own bindings';
}

done-testing;
