# rw writeback through a wrap chain needs shared container cells

Raku binds an `is rw` parameter to the caller's *container*, so in a wrap
chain every layer aliases one container: the wrapper's rw param, the
callsame'd original's rw param, and the caller's variable all observe a
single write.

```raku
sub w5($x is rw) { $x = $x + 1; $x }
&w5.wrap(sub ($y is rw) { my $r = callsame(); note "wrapper-y=$y"; "w5:$r" });
my $e = 40;
say w5($e);   # raku: wrapper-y=41 / w5:41 — the original's write is visible
say $e;       # raku: 41
```

mutsu implements rw as a bind-time value copy plus an exit writeback
(`apply_rw_bindings_to_env`), not a shared container. The chain relay
therefore only survives by *coincidence*: when the wrapper's and the
original's rw params share a name (`$x`/`$x`), the interpreter carrier's
blanket same-name env merge leaks the original's param value into the
wrapper's, and the wrapper's own exit writeback then relays the right
value. Rename the wrapper's param (`$y`, above) and mutsu prints
`wrapper-y=40` and leaves `$e` at 40 — verified 2026-08-05 on `main`
(pre-C6d-4), so this is NOT a C6d-4 regression; the writeback is
last-writer-wins over stale copies on every path.

Because the compiled-closure path does not reproduce the same-name leak,
the ADR-0019 C6d-4 fork (`call_sub_value` running a routine code object's
plan bytecode) is **gated off for routines with a scalar rw/raw param** —
remove that gate in `resolution_call_sub.rs` when fixing this.

Fix direction: bind a scalar `is rw` param as a shared `ContainerRef` cell
chained to the caller's (or outer rw param's) cell, so body writes go
through the cell and no exit copy-back is needed — the sound-mechanism
choice CLAUDE.md's gain/risk section prescribes (snapshot + writeback is
exactly the flaky-under-analysis-gaps shape). `apply_rw_bindings_to_env`
already stores *through* a cell when one exists; what is missing is the
binder creating/propagating cells for rw params in the first place, on
both the interpreter-carrier and compiled paths.

Related: `todo/tickets/rw-writeback-through-nontrivial-proto-body-is-lost.md`
(the proto `{*}` relay is the same missing-alias family).
