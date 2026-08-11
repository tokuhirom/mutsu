# Nested `whenever` registration no longer clobbers a supply block's shared hash/array cell

`supply { }` bodies box their own `my`-declared lexicals into a shared
`ContainerRef` cell (`share_supply_block_lexicals`) so every dispatch of the
enclosing `whenever` callback mutates one binding instead of a private
per-dispatch snapshot. The `%h{$k} = $v` / `@a[$i] = $v` "write through the
cross-thread shared store" fast paths
(`assign_hash_elem_to_shared_var`/`assign_array_elem_to_shared_var`,
`src/runtime/runtime_shared_vars.rs`) did not know about that cell: they only
recognised a bare `ValueView::Hash`/`ValueView::Array`. Once anything
activated `shared_vars_active` on the interpreter — in practice, a nested
`whenever <Promise>` registered mid-event, which always goes through
`clone_for_thread()` — a later element write on the boxed variable found the
value under its shared-store key was a `ContainerRef`, didn't match the
expected shape, silently treated the variable as *unshared*, and reinstalled
a brand-new empty `Hash`/`Array` into `env` — permanently un-boxing the cell
and discarding every key/element written before that point.

## Repro (dependency-free, deterministic, single-threaded)

```raku
my $trigger = Supplier.new;
my $done = Promise.new;
my $s = supply {
    my %streams;
    whenever $trigger.Supply -> $sid {
        unless %streams{$sid}:exists {
            %streams{$sid} = "S$sid";
            my $cancellation = Promise.new;
            whenever $cancellation { note "cancelled" }   # <-- the poison
        }
        note "after write $sid: keys={%streams.keys.sort.join(',')}";
        emit $sid;
    }
};
$s.tap: -> $v { $done.keep if $v == 5 };
$trigger.emit(3);
$trigger.emit(5);
await Promise.anyof($done, Promise.in(3));
```

Before the fix: raku prints `after write 5: keys=3,5`; mutsu printed
`after write 5: keys=5` — the event-3 entry was gone. After the fix both
match. Four bisect variants (nested `whenever` on every event vs. only one,
registered before vs. after the write) all match raku now; pin test
`t/supply-nested-whenever-hash-elem-cell.t`.

## Fix

`assign_hash_elem_to_shared_var`/`assign_array_elem_to_shared_var` now bail
out (`return None`) as soon as the variable's current value is already a
`ContainerRef`, before consulting the `__mutsu_atomic_hash::`/
`__mutsu_atomic_arr::` shared-store lane at all. The general element-assignment
path already knows how to write through a `ContainerRef` cell correctly (it's
how the very first write into a freshly-boxed hash always worked) — the cell
already provides everything the shared-store lane exists to provide
(cross-alias mutation visibility), so once boxed, routing back through the
lane was both redundant and, as it turned out, actively destructive.

## Cro impact

Root cause of the sole `http2-response-parser.rakutest` failure (now fully
green, 9/9) and of the `%streams` corruption inside the vendored Cro::HTTP2
`http2-request-parser.rakutest`'s stream demux
(`Cro::HTTP2::GeneralParser.transformer`, which does exactly this shape:
`%streams{$curr-sid} = Stream.new(...)` followed by a nested
`whenever $cancellation {...}`). Verified via a shadow-instrumented copy of
`GeneralParser` (`tmp/shadow/lib/Cro/HTTP2/GeneralParser.rakumod`,
`tmp/h2rp-probe.raku`): after the fix, `%streams` state and DATA-frame
routing match raku exactly at every checkpoint. `http2-request-parser.rakutest`
still has one residual failure, but it is now a **different, independent**
bug (verified to reproduce identically on the pre-fix binary too, so it was
simply masked before) — tracked separately in
`todo/deep/second-preserving-instance-body-blob-returns-empty-in-same-supply-body.md`.

Note: `@`/`%` aggregates are explicitly out of ADR-0025 slice 1/2 scope (the
capture-cell campaign covers plain `$` scalars) — this was its own bug in the
cross-thread shared-store lane, not something slice 2 would have caught.
