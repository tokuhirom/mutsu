use Test;

plan 10;

# Slice F (env<->locals coherence, docs/env-locals-coherence.md), closure
# category: a closure that mutates a *captured* outer variable writes the new
# value into env under the captured name; the call-site op now writes it
# straight through to the caller's local slot, so the caller observes the
# mutation without the reverse `sync_locals_from_env` pull. Covers the VM
# closure-dispatch paths: a directly-invoked block, an array `.map` block, a
# pointy block, and a per-iteration closure. (Range `.map` / `.grep`, which
# route through the interpreter dispatch, are a separate follow-up.)

# --- directly-invoked block mutating a captured scalar ---------------------
{
    my $c = 0;
    my $blk = { $c++ };
    $blk(); $blk(); $blk();
    is $c, 3, 'directly-invoked block accumulates into the captured scalar';
    is $c + 1, 4, 'caller slot stays coherent for a following read';
}

# --- array .map block accumulating into a captured scalar ------------------
{
    my $sum = 0;
    my @a = 1, 2, 3, 4;
    @a.map({ $sum += $_ });
    is $sum, 10, 'array .map block accumulates into the captured scalar';
    is @a.join(','), '1,2,3,4', 'the source array is unchanged';
}

# --- pointy block mutating a captured scalar -------------------------------
{
    my $n = 10;
    my $add = -> $x { $n += $x };
    $add(5);
    is $n, 15, 'pointy block mutates the captured outer scalar';
    $add(3);
    is $n, 18, 'a second call sees the updated captured value';
}

# --- closure created inside a loop, capturing the loop topic ---------------
{
    my $k = 0;
    my @b = 10, 20;
    for @b -> $v {
        my $f = { $k += $v };
        $f();
    }
    is $k, 30, 'per-iteration closure accumulates into an outer captured scalar';
}

# --- captured array mutated through .push inside a block -------------------
{
    my @acc;
    my $collect = -> $x { @acc.push($x) };
    $collect(1); $collect(2); $collect(3);
    is @acc.join(','), '1,2,3', 'block pushing into a captured array writes back';
}

# --- captured scalar updated and then read inside the SAME caller ----------
{
    my $total = 0;
    my $tick = { $total += 10 };
    $tick();
    my $half = $total / 2;
    $tick();
    is $total, 20, 'captured scalar coherent across interleaved caller reads';
    is $half, 5, 'the interleaved read saw the post-first-call value';
}
