use Test;

# ADR-0062: the legacy name-keyed atomic lane must anchor a newly created
# generation to the process-global PUBLISHED value, never to the acting
# thread's private `env` snapshot.
#
# Every case below needs a thread that is spawned BEFORE a write and runs its
# own atomic op AFTER it. Ordinary sequential-`await` code never produces that
# ordering, which is why the pre-existing pins
# (t/cross-thread-shared-var-writeback-coherence.t,
# t/atomic-cell-shape-refusal-symmetry.t) all passed while this was broken.
# The `Channel` is what forces it: the worker blocks on `.receive` until the
# main thread has finished writing.

plan 7;

# --- 1. 2-arg cas, identity block, with a priming cas on the main thread ---
# (the shape the originating ticket described: the priming cas creates lane
# generation 1, `$x = 4` retires it, and the worker creates generation 2)
{
    my $x = 1;
    my $go = Channel.new;
    my $pB = start { $go.receive; cas $x, -> $v { $v } };
    cas $x, -> $v { $v };
    $x = 4;
    Promise.allof(start { $x = 5 }).result;
    $go.send(1);
    $pB.result;
    is $x, 5, 'stale-spawned identity cas does not clobber a newer value (after a priming cas)';
}

# --- 2. the same with NO priming cas ---
# The worker creates the FIRST lane generation. Proves the defect is the
# seeding source, not the retire/resurrect cycle.
{
    my $x = 1;
    my $go = Channel.new;
    my $pB = start { $go.receive; cas $x, -> $v { $v } };
    $x = 4;
    Promise.allof(start { $x = 5 }).result;
    $go.send(1);
    $pB.result;
    is $x, 5, 'stale-spawned identity cas creating the first lane generation reads the published value';
}

# --- 3. 3-arg cas: the compare itself must see the published value ---
# The worker expects its own stale snapshot (1); the real value is 5, so the
# swap must FAIL and return 5.
{
    my $x = 1;
    my $go = Channel.new;
    my $pB = start { $go.receive; cas $x, 1, 99 };
    $x = 4;
    Promise.allof(start { $x = 5 }).result;
    $go.send(1);
    my $seen = $pB.result;
    is $seen, 5, '3-arg cas from a stale thread compares against the published value';
    is $x, 5, '3-arg cas from a stale thread does not swap in over a newer value';
}

# --- 4. three stale-spawned threads, not two ---
{
    my $x = 1;
    my $go = Channel.new;
    my @p = (^3).map: { start { $go.receive; cas $x, -> $v { $v } } };
    $x = 4;
    Promise.allof(start { $x = 5 }).result;
    $go.send($_) for ^3;
    .result for @p;
    is $x, 5, 'three stale-spawned identity cas threads all read the published value';
}

# --- 5/6. the cell-promoted shape (`atomicint`) must hold too ---
# `my atomicint $x` takes the ContainerRef cell lane rather than the legacy
# name-keyed lane, so it was already correct; it is pinned here so the two
# lanes stay symmetric.
{
    my atomicint $x = 1;
    my $go = Channel.new;
    my $pB = start { $go.receive; $x⚛++ };
    $x = 4;
    Promise.allof(start { $x = 5 }).result;
    $go.send(1);
    $pB.result;
    is $x, 6, 'cell-lane atomicint fetch-inc from a stale thread increments the published value';
}

{
    my atomicint $x = 1;
    my $go = Channel.new;
    my $pB = start { $go.receive; atomic-fetch-add($x, 10) };
    $x = 4;
    Promise.allof(start { $x = 5 }).result;
    $go.send(1);
    $pB.result;
    is $x, 15, 'cell-lane atomic-fetch-add from a stale thread adds to the published value';
}
