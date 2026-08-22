# A source quit now propagates through any depth of chained on-demand `whenever` sources

`todo/deep/nested-on-demand-whenever-quit-propagation-gap.md` (filed 2026-08-19
while writing ADR-0031 Slice 2's regression coverage) is closed. A `quit`
travelling through two or more levels of chained on-demand supply blocks reached
nothing: the outermost tap's `quit =>` handler never fired, and neither did
`done`, so the pipeline simply sat open forever.

```raku
my $sup = Supplier.new;
my $src = supply { whenever $sup.Supply -> $v { emit $v } }
my $out = supply { whenever $src        -> $v { emit $v } }
my $died = False;
$out.tap({ }, quit => { $died = True });
$sup.emit('g1');
$sup.quit("boom");
# raku:  died=True     mutsu (before): died=False
```

## Root cause

The `"tap" | "act"` dispatch's b3 branch (`src/runtime/native_supply_mut_methods.rs`,
"chain a REAL tap so liveness propagates") builds the argument list for the
recursive `.tap()` on the inner on-demand source. It registered a `"quit"` pair
on that inner tap **only when the `whenever` had declared a `QUIT` phaser of its
own**:

```rust
if let Some(q) = quit_cbs.first() {
    tap_args.push(Value::pair("quit".to_string(), q.clone()));
}
```

`whenever $src -> $v { emit $v }` declares no `QUIT` phaser, so `quit_cbs` was
empty and *no* quit callback was registered on the inner supply at all. The
quit-propagation machinery ADR-0031 Slice 1 built — a supplier-backed source's
own `.quit()` reaching the enclosing block's emitter through the serialize-group
link (`take_supplier_quit_callbacks_via_group`) — is only a **one-hop** lookup,
and it found an empty list at the first hop because nothing had ever been
registered there. The quit was silently swallowed.

This is asymmetric with how the same branch already handles `done`: `done_chain`
is registered unconditionally, carrying the done-group marker whether or not a
`LAST` phaser was declared.

## The fix

b3 now registers a `quit =>` on the inner tap unconditionally, and what it
registers re-derives the destination from ADR-0031 Decision A's principle rather
than from whichever upstream object happened to be in hand: the enclosing supply
block's own `emitter_supplier_id`.

The new `__SupplyQuitForwarder`
(`src/runtime/native_methods/supply_quit_forwarder.rs`) is a real synthesized
callable — the established empty-env `SubData` / literal-internal-instance idiom
from `__ScheduledTapPump` (ADR-0028 §2) and `__SupplyCollector` (ADR-0031
Slice 2) — so every quit-delivery path dispatches it uniformly through
`call_sub_value`, including the channel-backed act loop that invokes a tap's
quit callback directly instead of through `call_supply_quit_handler`. It carries
the `whenever`'s own `QUIT` phaser callbacks and runs the full `QuitOutcome`
protocol on them first: a handled quit completes the enclosing block with `done`
(unless the phaser already called `done` itself), an unhandled one quits the
block's emitter through the canonical `Supplier."quit"`.

Because each level's tap installs the same forwarder one level further out,
propagation is transitive to any nesting depth — no serialize-group chain walk
was needed, which is what the ticket had feared would be required.

Two behaviours besides the reported one were fixed by the same change, both
cross-checked against real `raku` first:

- a `QUIT` phaser at a *chained* level that handles the exception now completes
  the enclosing supply with `done` (previously neither `done` nor `quit` fired);
- `.list` / `.wait` over such a pipeline now throws the source's quit instead of
  returning the values collected so far — and, for a source that quits without
  closing its taps, without waiting out the ADR-0031 Slice 2 drain deadline.

Pinned by `t/supply-nested-on-demand-quit-propagation.t` (13 assertions covering
two levels, three levels, a handling `QUIT` phaser, the unchanged single-level
path, and the `.list` shape), which passes verbatim under `raku`.
