use v6;
use Test;

plan 5;

# ADR-0031 Decision B (Slice 2): `supply_get_values` (the `.list`/`.List`/
# `.Seq`/`.wait`/combinator chokepoint) now taps and drains the supply it is
# asked to materialize, instead of the old synchronous replay
# (`replay_cold_whenever_capture` / `replay_static_whenever_promise`, both
# retired). The old replay could only observe a nested `whenever` source's
# values that were already available *during* the synchronous body call, so
# a `supply { whenever <live source> { emit ... } }` nested inside another
# `supply { whenever <that> { emit ... } }` silently lost every value
# emitted asynchronously after the outer materialization call returned.
# Tap-and-drain sees them, because it genuinely subscribes and waits for the
# source's own `done`/`quit` instead of reading a snapshot.
#
# probe5 case E from the ADR (the ticket's own repro), via `.list`:
# `todo/deep/cold-supply-whenever-source-replayed-not-tapped.md`.
{
    my $supE = Supplier.new;
    my $srcE = supply { whenever $supE.Supply -> $v { emit $v } }
    my $outE = supply { whenever $srcE -> $v { emit $v } }
    start { sleep 0.05; $supE.emit('e1'); $supE.emit('e2'); $supE.done }
    is $outE.list, ('e1', 'e2'), 'probe5 case E: .list drains values emitted after the tap call returns';
}

# Same shape through `.wait` (returns the last value, not the whole list).
{
    my $supF = Supplier.new;
    my $srcF = supply { whenever $supF.Supply -> $v { emit $v } }
    my $outF = supply { whenever $srcF -> $v { emit $v } }
    start { sleep 0.05; $supF.emit('f1'); $supF.emit('f2'); $supF.done }
    is $outF.wait, 'f2', '.wait on the same live-inner-subscription shape returns the last value';
}

# A combinator (`.sort`) that goes through the same `supply_get_values`
# chokepoint also sees a live inner subscription's later values, not just
# the direct `.list` feeder.
{
    my $supH = Supplier.new;
    my $srcH = supply { whenever $supH.Supply -> $v { emit $v } }
    my $outH = supply { whenever $srcH -> $v { emit $v } }
    start { sleep 0.05; $supH.emit(3); $supH.emit(1); $supH.emit(2); $supH.done }
    is $outH.sort.list, (1, 2, 3), '.sort (a supply_get_values combinator) also drains a live inner subscription';
}

# A finite, static nested whenever source (no live subscription at all)
# still materializes synchronously through the same tap-and-drain path —
# tap-and-drain must not turn an already-fast case into a slow one.
{
    my $sup-static = supply { whenever Supply.from-list(1, 2, 3) -> $v { emit $v * 10 } }
    is $sup-static.list, (10, 20, 30), 'a purely static nested whenever source still materializes synchronously';
}

# `.head` on an actually-infinite, channel-backed live Supply must not tap
# the whole thing to materialize a value list it never even reads for that
# branch (regression pin for the `.head` fix this refactor needed: calling
# `supply_get_values` unconditionally there would tap-and-block on
# `Supply.interval` for the full drain deadline before reaching the
# live-source branch that ignores the result).
{
    my @res;
    my $done;
    Supply.interval(0.05).head(3).tap({ @res.push($_) }, :done({ $done = True }));
    for ^100 { last if $done; sleep .05 }
    is @res.elems, 3, '.head(3) on an infinite channel-backed Supply still completes promptly';
}
