# Slice 1b (env<->locals cross-frame cell coherence): a `submethod DESTROY`
# fires on a *worker* thread (a `start` block spawns a cloned interpreter), and
# writes a captured-outer lexical owned by a top-level local slot. The worker's
# write propagates to the parent env via the shared-var sync, but the parent's
# matching local *slot* lives several frames up (the `await` runs
# `run_pending_instance_destroys()` with empty `locals` in between). A drop-on-miss
# writeback list would be consumed and discarded by those intervening frames before
# reaching the owner. The retain-on-miss caller-var list carries it up to the
# owning frame. This pin reproduces the destruction.t failure that only manifested
# with blanket reconcile OFF (MUTSU_NO_BLANKET_RECONCILE=1).
use Test;
plan 5;

# 1. Minimal repro from docs/captured-outer-cell-sharing.md §7.2c: a scalar
#    DESTROY write inside `await start { loop {...} }` with conditional creation.
{
    my $a = 0;
    my @order;
    class Foo1 { submethod DESTROY { $a++ } }
    class Bar1 { submethod DESTROY { push @order, "x" } }
    my $b0 = Bar1.new; $b0 = Nil;
    await start {
        loop {
            $*VM.request-garbage-collection;
            my $foo = Foo1.new;
            my $bar = Bar1.new unless +@order;
            last if $a && @order;
        }
    };
    ok $a >= 1, "DESTROY scalar write from worker thread reaches top-level slot ($a)";
}

# 2. A simple cross-thread captured scalar write through `await start { ... }`.
{
    my $x = 10;
    await start { $x = 42 };
    is $x, 42, "cross-thread scalar write reaches caller slot";
}

# 3. A worker that writes a captured scalar inside a loop, awaited.
{
    my $sum = 0;
    await start {
        for 1..5 { $sum += $_ }
    };
    is $sum, 15, "cross-thread accumulation in worker loop reaches caller slot";
}

# 4. Two captured vars (scalar + array) written from the worker.
{
    my $count = 0;
    my @log;
    await start {
        for 1..3 {
            $count++;
            push @log, $count;
        }
    };
    is $count, 3, "worker scalar write coherent with caller slot";
    is @log.join(","), "1,2,3", "worker array write coherent with caller slot";
}
