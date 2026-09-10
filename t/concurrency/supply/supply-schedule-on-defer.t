use Test;

# ADR-0028: `Supply.schedule-on($scheduler)` must genuinely defer emit/done/
# quit delivery relative to the emitting thread instead of running the tap
# callback synchronously on the emitting call stack (the old no-op behavior).
# Every pin here is synchronized through a Promise -- no sleep-based timing
# assertions, so this cannot flake by construction.

plan 5;

# 1. The deep ticket's Cro-free deadlock repro (todo/deep/supply-schedule-
# on-does-not-defer-tap-dispatch.md, rebuilt and verified against real `raku`
# before pinning -- several plausible simplifications did NOT reproduce).
# A blocking `await` inside the schedule-on'd tap callback needs a sibling
# `start {}` statement (queued behind the emit that triggered the tap) to
# run before it can resolve. Without genuine deferral the emitting thread is
# stuck inside the synchronous tap callback and `$inner` is never kept, so
# the `await` only ever sees the 3s timeout branch (`Planned`).
{
    my $supplier = Supplier.new;
    my $inner = Promise.new;
    my $done = Promise.new;
    my $inner-status;
    $supplier.Supply.schedule-on($*SCHEDULER).tap: -> $v {
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
        "a blocking wait inside a schedule-on'd tap callback does not deadlock a sibling start{} emit";
}

# 2. Emission order and emit-before-done ordering through a real
# ThreadPoolScheduler pump. A naive per-emit pool submit would let two
# deliveries race across workers; the serialized single-drain-worker pump
# must keep them in order.
{
    my $supplier = Supplier.new;
    my @seen;
    my $done = Promise.new;
    $supplier.Supply.schedule-on(ThreadPoolScheduler.new).tap(
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
    is-deeply @seen, [1, 2, 3],
        "schedule-on(ThreadPoolScheduler) delivers emits in order before done";
}

# 3. Quit routing through the pump.
{
    my $supplier = Supplier.new;
    my $reason;
    my $quit = Promise.new;
    $supplier.Supply.schedule-on(ThreadPoolScheduler.new).tap(
        -> $v { },
        quit => -> $ex { $reason = $ex.message; $quit.keep(True) },
    );
    start { $supplier.quit(X::AdHoc.new(payload => "adr-0028-quit")); };
    await Promise.anyof($quit, Promise.in(5));
    is $reason, "adr-0028-quit",
        "schedule-on(ThreadPoolScheduler) routes quit to the quit => handler";
}

# 4. Tap.close on a scheduled tap stops delivery and reclaims the drain --
# a value emitted after close must not reach the (closed) callback.
{
    my $supplier = Supplier.new;
    my @seen;
    my $got-one = Promise.new;
    my $tap = $supplier.Supply.schedule-on(ThreadPoolScheduler.new).tap(-> $v {
        @seen.push($v);
        $got-one.keep(True) if $v == 1;
    });
    $supplier.emit(1);
    await Promise.anyof($got-one, Promise.in(5));
    $tap.close;
    $supplier.emit(2);
    sleep 0.2; # give a leaked drain a chance to misbehave before asserting
    is-deeply @seen, [1], "Tap.close on a schedule-on'd tap stops further delivery";
}

# 5. Any Scheduler that is not Current-/ThreadPool- routes through its own
# `.cue`, exactly like `Supply.interval`'s scheduler wiring -- delivery is
# queued, not bypassed with a hardcoded pool submit.
{
    my class QueueScheduler does Scheduler {
        has @.queue;
        method cue(&code, :$every, :$in = 0, *%unknown) {
            @!queue.push(&code) unless $every;
        }
        method loads() { @!queue.elems }
        method run-all() {
            my @q = @!queue;
            @!queue = ();
            .() for @q;
        }
    }
    my $supplier = Supplier.new;
    my $scheduler = QueueScheduler.new;
    my @seen;
    $supplier.Supply.schedule-on($scheduler).tap(-> $v { @seen.push($v) });
    $supplier.emit('x');
    my @before = @seen;
    $scheduler.run-all;
    is-deeply (@before, @seen), ([], ['x']),
        "schedule-on(a user Scheduler) queues delivery through its own .cue instead of running it inline";
}

# vim: expandtab shiftwidth=4
