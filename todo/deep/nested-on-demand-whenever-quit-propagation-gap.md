# A source quit through 2+ levels of nested on-demand `whenever` chaining is not propagated

Found 2026-08-19 while writing regression coverage for ADR-0031 Slice 2
(`docs/adr/0031-supply-quit-ownership-and-cold-source-tapping.md`, Decision
B: `supply_get_values` taps and drains instead of replaying).

## Repro

```raku
my $supG = Supplier.new;
my $srcG = supply { whenever $supG.Supply -> $v { emit $v } }
my $outG = supply { whenever $srcG -> $v { emit $v } }
my $died = False;
my @got;
$outG.tap({ @got.push($_) }, quit => { $died = True });
$supG.emit('g1');
$supG.quit("boom");
sleep 0.2;
say "died=$died got=@got.raku()";
# raku:  died=True  got=["g1"]
# mutsu: died=False got=["g1"]
```

This reproduces via plain `.tap()` — **it is unrelated to `supply_get_values`
or `.list`/`.wait`**, and is present before and after the Slice 2 change (it
is not a Slice 2 regression). It is a gap in ADR-0031 Slice 1 (Decision A,
quit ownership): the fix there only covers a **body `die` converting to a
quit** and a **direct** source's own `.quit()` reaching a tap registered
one level up. It does not cover a genuine source quit propagating through
**two or more levels** of chained on-demand `whenever` sources.

## Root cause (partial analysis)

`$outG`'s `whenever $srcG` is handled by the b3 "chain a REAL tap" branch in
`src/runtime/native_supply_mut_methods.rs` (the on-demand-source branch of
the `"tap"|"act"` dispatch). That branch builds `tap_args` for the recursive
`.tap()` call on `$srcG` and only adds a `"quit"` pair when the *whenever's
own* QUIT phaser array (`arr[3]`) is non-empty:

```rust
if let Some(q) = quit_cbs.first() {
    tap_args.push(Value::pair("quit".to_string(), q.clone()));
}
```

Since `whenever $srcG -> $v { emit $v }` has no `QUIT { ... }` block,
`quit_cbs` is empty, so **no** `"quit"` callback is registered on `$srcG`'s
own `.tap()` call at all. `$srcG`'s `whenever $supG.Supply` (b1,
supplier-backed) has its source quit routed via
`take_supplier_quit_callbacks_via_group`, which drains `$supG`'s own
`supplier_id` plus (via the serialize-group link) `$srcG`'s own emitter's
quit callbacks — but nothing was ever registered there (see above), so the
quit is silently swallowed: it never reaches `$outG`'s tap's `quit =>` at
all, and `$srcG`/`$outG` never complete either (no `done`, no `quit` — the
supply just sits open forever).

`supplier_serialize_group`/`take_supplier_quit_callbacks_via_group` is also
only a **one-hop** lookup (it does not walk a serialize-group chain
transitively), so even fixing the immediate gap above would need either a
second hop or (more likely) the b3 branch registering its OWN outer
`quit_cb` — not just the whenever's own QUIT-phaser array — on the
recursive `.tap()` call, the same way it already does for `"done"`
(`done_chain` unconditionally includes `last_cbs` + `done_group_marker`
regardless of whether the whenever declared its own LAST phaser).

## Why ADR-0031 Slice 2 made this more visible

Before Slice 2, `supply_get_values`'s old pull-based replay
(`replay_cold_whenever_capture`) recognized a *live* nested marker and just
`continue`d past it (`if is_live { continue; }`) — dropping it silently but
returning immediately. After Slice 2, `.list`/`.wait`/the ~20 combinators
genuinely tap-and-drain and **wait** for a done/quit signal that (because of
this gap) never arrives, so a program hitting this exact shape through
`.list`/`.wait` now blocks for the full 30s drain deadline instead of
returning a fast (if silently wrong) answer. `.tap()` itself does not block
(it only registers callbacks and returns), so the `.tap()` repro above does
not hang — it just reports the wrong `died` value forever.

## Affected files

- `src/runtime/native_supply_mut_methods.rs` — the b3 "chain a REAL tap"
  branch's `tap_args` construction (the `quit_cbs.first()` gate).
- `src/runtime/native_methods/state_supplier.rs` — `set_supplier_serialize_group`
  / `take_supplier_quit_callbacks_via_group` (currently a single hop).

## Why it is deep

Fixing the immediate gap (always forward the outer `quit_cb` from b3, not
just the whenever's own QUIT-phaser array) only closes one hop. A chain three
or more `supply { whenever <on-demand> { ... } }` levels deep needs either a
transitive serialize-group walk or (architecturally cleaner, matching
ADR-0031's "the emitter is the only object that means 'this supply'"
principle) each level's quit registration to be re-derived from first
principles rather than patched incrementally. This likely wants its own
ADR-0031-style design pass rather than a quick patch, given Slice 1 already
had to correct a subtly wrong first attempt at quit ownership (see that
ADR's "Outcome" section) — the interaction between chained on-demand taps
and the emitter-owns-quit model needs to be worked out deliberately, not
guessed at under this ticket.

## Impact

Low observed impact today: no currently-whitelisted roast test or `t/` test
hits this shape (it was found while writing new Slice 2 regression
coverage, and the test case exercising it was deliberately left out of
`t/supply-cold-whenever-live-inner-drain.t` to avoid a 30s-bounded hang in
the suite). Worth fixing before more `Test::Scheduler`/Cro-style pipelines
chain three or more on-demand `supply` blocks together, since a real
producer failure in that shape would now hang the consumer's `.list`/`.wait`
for 30s instead of just misreporting.
