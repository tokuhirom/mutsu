# `supply_get_values` now taps and drains a cold on-demand supply instead of replaying it

Implemented ADR-0031 Decision B / Slice 2
(`docs/adr/0031-supply-quit-ownership-and-cold-source-tapping.md`): the
`supply_get_values` chokepoint every `.list`/`.List`/`.Seq`/`.wait`/
combinator (`.sort`, `.head`, `.flat`, `.batch`, `.rotor`, `.zip`, `.start`,
...) ultimately reads from now taps and drains an on-demand `supply { ... }`
block instead of pull-replaying it, closing the last of the deep ticket's
two defects (`todo/deep/cold-supply-whenever-source-replayed-not-tapped.md`,
now retired — its Defect A half was already fixed by Slice 1).

Previously, materializing an on-demand supply's values walked
`run_on_demand_body`'s raw emitted markers by hand: a nested cold `whenever`
source was replayed synchronously (`replay_cold_whenever_capture`), and a
*live* nested subscription — one whose own upstream had not finished
emitting by the time the synchronous walk finished — was silently dropped
(`if is_live { continue; }`). That meant a pipeline like:

```raku
my $sup = Supplier.new;
my $src = supply { whenever $sup.Supply -> $v { emit $v } }
my $out = supply { whenever $src -> $v { emit $v } }
start { sleep 0.05; $sup.emit('e1'); $sup.emit('e2'); $sup.done }
say $out.list;   # raku: (e1 e2)   mutsu (before): ()
```

lost every value emitted after the synchronous replay call returned.

The fix: `Interpreter::supply_collect_values` taps the supply for real
(reusing the `"tap"|"act"` dispatch that already drives all four
whenever-source branches correctly, including live ones) with a synthesized
`__SupplyCollector` shim — the same "empty-env callable whose body is one
`MethodCall` on a literal internal instance" idiom ADR-0028's
`__ScheduledTapPump` established — and drains the resulting events through a
`ReactWaker`, blocking (bounded by a 30s deadline) only until the tap
actually signals done or quit. `supply_get_values` now dispatches to this
tap-and-drain path only for an on-demand `supply { ... }` block — the only
shape that can ever contain a nested `whenever` marker in the first place —
and keeps the old direct attribute read for a plain values-array or live
Supplier-/channel-backed Supply, since neither can ever hide a dropped
subscription and routing them through a callback bind would (confirmed
against `raku`) itemize array values on the way through, breaking `.flat`,
and could block on a genuinely infinite live source a caller like `.head`
never actually reads the materialized values for.

`replay_cold_whenever_capture` and `replay_static_whenever_promise` are
retired: their "materialize the source" halves are superseded by
`supply_get_values`'s new dispatch, and their "drive the whenever body over
the values" halves survive under new names
(`drive_whenever_body_over_values` / `drive_whenever_promise_over_values`),
now taking pre-materialized values instead of pulling from a source
themselves.

New test: `t/supply-cold-whenever-live-inner-drain.t` — the fix's own probe
(the repro above, via `.list`, `.wait`, and `.sort`), a static-source
regression pin, and a pin for the `.head`-on-an-infinite-channel-backed-Supply
case the narrowing above exists to protect. Verified against the full
`t/supply-*.t` / `t/whenever-*.t` / `t/react-*.t` / `t/promise-supply-*.t`
suites, the full `t/` TAP suite, and every whitelisted `roast/S17-supply/*`
file on a release build.

While verifying the fix, a separate, pre-existing gap was found and filed
(`todo/deep/nested-on-demand-whenever-quit-propagation-gap.md`): a source
quit through two or more levels of chained on-demand `whenever` sources does
not propagate, reproducible via plain `.tap()` alone and unrelated to
`supply_get_values`. It is not part of this fix, but tap-and-drain makes its
symptom more visible for `.list`/`.wait` specifically (a fast-but-wrong
answer becomes a bounded 30s wait instead), which is why it is now tracked.
