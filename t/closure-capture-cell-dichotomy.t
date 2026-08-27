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

plan 11;

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
# NOTE: only the slot-resident variant is pinned. The env-resident variant --
# the same program with `my $g = { $b }` added to `collide`, which forces the
# caller's colliding lexical out of its local slot and into `env` -- STILL
# RETURNS `CALLER` and is the open half of ADR-0055 section 1.2(b). Closing it
# needs the vouch/cell dichotomy extended to read-only call-arg-source captures,
# which is blocked on
# `todo/deep/unvouched-capture-cells-leak-state-across-cro-client-requests.md`.
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

done-testing;
