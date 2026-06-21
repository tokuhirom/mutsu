use Test;

# Writing a caller frame's lexical by name — `$CALLER::x = v` or
# `callframe(d).my.<$x> = v` — must refresh the caller's local *slot*, not just
# its env entry, so the caller reads the new value (Sub-slice 1b).
#
# Root cause this pins: `set_caller_var` wrote the value into the caller env
# (propagated on frame pop) but not the caller's local slot. With blanket
# reconcile off (the single-store path) the caller read the stale slot. The fix
# records the bare name on a retain-on-miss caller-var writeback list so the
# call site drains env -> slot, surviving an intervening *deeper* call the
# writer makes before returning. Verified ON and OFF (MUTSU_NO_BLANKET_RECONCILE).

plan 5;

# $CALLER:: read-modify-write of a dynamic var.
{
    my sub modify { $CALLER::foo++ }
    my $foo is dynamic = 42;
    modify();
    is $foo, 43, '$CALLER::foo++ modifies the caller lexical';
}

# callframe(1).my.<$x> = v writes the caller's dynamic lexical.
{
    sub f1() { callframe(1).my.<$x> = 23 }
    my $x is dynamic = 42;
    f1();
    is $x, 23, 'callframe(1).my.<$x> = v modifies the caller lexical';
}

# The write must survive an intervening *deeper* call made before the writer
# returns (the pending writeback must not be consumed one frame too soon).
{
    sub deeper() { 99 }
    sub f2() {
        callframe(1).my.<$y> = 7;
        deeper();           # intervening deeper call
    }
    my $y is dynamic = 0;
    f2();
    is $y, 7, 'caller-frame write survives an intervening deeper call';
}

# A non-dynamic caller lexical cannot be mutated this way (still dies).
{
    sub f3() { callframe(1).my.<$z> = 5 }
    my $z = 100;
    dies-ok { f3() }, 'cannot mutate a non-dynamic caller lexical';
    is $z, 100, 'non-dynamic caller lexical unchanged';
}
