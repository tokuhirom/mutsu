use Test;

# ADR-0028 / ADR-0043: `.map`/`.grep`/`.do`/`.flat` applied to a
# `.schedule-on()`'d Supply must genuinely defer delivery to the *user's*
# tap the same way a bare `.schedule-on(...).tap(...)` does (see
# t/supply-schedule-on-defer.t). `make_live_transform_supply` and the "flat"
# arm's live branch built a fresh downstream attrs map without copying
# forward the `"scheduler"` attribute -- so the derived Supply's tap never
# saw a scheduler and delivered synchronously, deadlocking a schedule-on'd
# tap that blocks on a sibling `start {}` (todo/tickets/scheduled-supply-
# derived-transform-ops-drop-scheduler-attr.md).
# Every pin here is synchronized through a Promise -- no sleep-based timing
# assertions, so this cannot flake by construction.

plan 7;

# 1. .map
{
    my $supplier = Supplier.new;
    my $inner = Promise.new;
    my $done = Promise.new;
    my $inner-status;
    $supplier.Supply.schedule-on(ThreadPoolScheduler.new).map(-> $v { $v }).tap: -> $v {
        await Promise.anyof($inner, Promise.in(3));
        $inner-status = $inner.status;
        $done.keep(True);
    };
    start {
        $supplier.emit('x');
        $inner.keep(True);
    };
    await Promise.anyof($done, Promise.in(5));
    ok $inner-status === Kept,
        ".schedule-on(...).map(...) does not deadlock a sibling start{} emit";
}

# 2. .grep
{
    my $supplier = Supplier.new;
    my $inner = Promise.new;
    my $done = Promise.new;
    my $inner-status;
    $supplier.Supply.schedule-on(ThreadPoolScheduler.new).grep(-> $v { True }).tap: -> $v {
        await Promise.anyof($inner, Promise.in(3));
        $inner-status = $inner.status;
        $done.keep(True);
    };
    start {
        $supplier.emit('x');
        $inner.keep(True);
    };
    await Promise.anyof($done, Promise.in(5));
    ok $inner-status === Kept,
        ".schedule-on(...).grep(...) does not deadlock a sibling start{} emit";
}

# 3. .do
{
    my $supplier = Supplier.new;
    my $inner = Promise.new;
    my $done = Promise.new;
    my $inner-status;
    $supplier.Supply.schedule-on(ThreadPoolScheduler.new).do(-> $v { }).tap: -> $v {
        await Promise.anyof($inner, Promise.in(3));
        $inner-status = $inner.status;
        $done.keep(True);
    };
    start {
        $supplier.emit('x');
        $inner.keep(True);
    };
    await Promise.anyof($done, Promise.in(5));
    ok $inner-status === Kept,
        ".schedule-on(...).do(...) does not deadlock a sibling start{} emit";
}

# 4. .flat
{
    my $supplier = Supplier.new;
    my $inner = Promise.new;
    my $done = Promise.new;
    my $inner-status;
    $supplier.Supply.schedule-on(ThreadPoolScheduler.new).flat.tap: -> $v {
        await Promise.anyof($inner, Promise.in(3));
        $inner-status = $inner.status;
        $done.keep(True);
    };
    start {
        $supplier.emit('x');
        $inner.keep(True);
    };
    await Promise.anyof($done, Promise.in(5));
    ok $inner-status === Kept,
        ".schedule-on(...).flat does not deadlock a sibling start{} emit";
}

# 5. Emission order survives the derived-transform + scheduled-pump path.
{
    my $supplier = Supplier.new;
    my @seen;
    my $done = Promise.new;
    $supplier.Supply.schedule-on(ThreadPoolScheduler.new).map(* * 10).tap(
        -> $v { @seen.push($v) },
        done => { $done.keep(True) },
    );
    start {
        $supplier.emit(1);
        $supplier.emit(2);
        $supplier.emit(3);
        $supplier.done;
    };
    await Promise.anyof($done, Promise.in(5));
    is-deeply @seen, [10, 20, 30],
        ".schedule-on(ThreadPoolScheduler).map(...) delivers emits in order";
}

# 6. Negative case: CurrentThreadScheduler must NOT defer -- delivery stays
# synchronous, so the same blocking-await shape still deadlocks (Planned),
# exactly as it does for a bare .schedule-on(CurrentThreadScheduler.new)
# with no transform. This guards against a fix that defers unconditionally
# instead of copying forward whatever scheduler (or its absence) applies.
{
    my $supplier = Supplier.new;
    my $inner = Promise.new;
    my $done = Promise.new;
    my $inner-status;
    $supplier.Supply.schedule-on(CurrentThreadScheduler.new).map(-> $v { $v }).tap: -> $v {
        await Promise.anyof($inner, Promise.in(3));
        $inner-status = $inner.status;
        $done.keep(True);
    };
    start {
        $supplier.emit('x');
        $inner.keep(True);
    };
    await Promise.anyof($done, Promise.in(5));
    ok $inner-status === Planned,
        ".schedule-on(CurrentThreadScheduler).map(...) stays synchronous (no deferral)";
}

# 7. Tap.close cascades through the derived (.map) Supply to stop delivery.
{
    my $supplier = Supplier.new;
    my @seen;
    my $got-one = Promise.new;
    my $tap = $supplier.Supply.schedule-on(ThreadPoolScheduler.new).map(-> $v { $v }).tap(-> $v {
        @seen.push($v);
        $got-one.keep(True) if $v == 1;
    });
    $supplier.emit(1);
    await Promise.anyof($got-one, Promise.in(5));
    $tap.close;
    $supplier.emit(2);
    sleep 0.2; # give a leaked drain a chance to misbehave before asserting
    is-deeply @seen, [1],
        "Tap.close on a schedule-on'd .map(...) tap stops further delivery";
}

# vim: expandtab shiftwidth=4
