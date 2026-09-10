use Test;

# ADR-0028 Slice 2: `Supply.schedule-on($scheduler)` must genuinely defer
# delivery not only through a direct `.tap()`/`.act()` (Slice 1), but also
# through registration paths that bypass that chokepoint entirely --
# `whenever $scheduled-supply { ... }` inside another `supply { }` block
# (registers its body callback directly via `register_supplier_tap`), and
# deferred-registration derived operators (`.lines`, `.words`, `.unique`,
# `.elems`, `.produce`) applied AFTER `.schedule-on()`, which build a fresh
# attrs map that must carry the `"scheduler"` attribute forward so the
# eventual `.tap()` still classifies it. Every pin here is synchronized
# through a Promise -- no sleep-based timing assertions for the positive
# cases, so this cannot flake by construction.

plan 3;

# 1. whenever $scheduled-supply { ... } inside another supply block: a
# blocking `await` inside the whenever body must not deadlock a sibling
# `start {}` emit, exactly like the direct-tap Slice-1 repro.
{
    my $supplier = Supplier.new;
    my $inner = Promise.new;
    my $done = Promise.new;
    my $inner-status;

    my $s = supply {
        whenever $supplier.Supply.schedule-on(ThreadPoolScheduler.new) -> $v {
            await Promise.anyof($inner, Promise.in(3));
            $inner-status = $inner.status;
            $done.keep(True);
        }
    };
    $s.tap(-> $v { });

    start {
        $supplier.emit('x');
        $inner.keep(True);
    };
    await Promise.anyof($done, Promise.in(5));
    ok $inner-status === Kept,
        "whenever on a schedule-on'd nested source defers delivery instead of deadlocking a sibling start{} emit";
}

# 2. A deferred-registration derived operator (.lines) applied AFTER
# .schedule-on must still defer: the "scheduler" attribute has to survive
# into the fresh attrs .lines builds so the eventual .tap() classifies it.
{
    my $supplier = Supplier.new;
    my $inner = Promise.new;
    my $done = Promise.new;
    my $inner-status;

    $supplier.Supply.schedule-on(ThreadPoolScheduler.new).lines.tap: -> $v {
        await Promise.anyof($inner, Promise.in(3));
        $inner-status = $inner.status;
        $done.keep(True);
    };
    start {
        $supplier.emit("x\n");
        $inner.keep(True);
    };
    await Promise.anyof($done, Promise.in(5));
    ok $inner-status === Kept,
        ".lines applied after schedule-on(ThreadPoolScheduler) still defers delivery";
}

# 3. Closing the outer tap on a supply block whose whenever taps a
# schedule-on'd nested source must reclaim that nested pump too (no leaked
# drain worker still delivering after close).
{
    my $supplier = Supplier.new;
    my @seen;
    my $got-one = Promise.new;

    my $s = supply {
        whenever $supplier.Supply.schedule-on(ThreadPoolScheduler.new) -> $v {
            @seen.push($v);
            $got-one.keep(True) if $v == 1;
        }
    };
    my $tap = $s.tap(-> $v { });
    $supplier.emit(1);
    await Promise.anyof($got-one, Promise.in(5));
    $tap.close;
    $supplier.emit(2);
    sleep 0.2; # give a leaked drain a chance to misbehave before asserting
    is-deeply @seen, [1],
        "closing the outer tap reclaims a nested whenever's schedule-on pump";
}

# vim: expandtab shiftwidth=4
