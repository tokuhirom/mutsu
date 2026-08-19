# A cold `supply` used as a `whenever` source: the surviving gaps are quit propagation, plus `supply_get_values`'s replay

Found 2026-07-25 in Test::Scheduler (`TODO_dist` T-037). Narrowed 2026-07-25
after the *nested* half of it was fixed (pin `t/supply-nested-whenever-promise.t`).
**Re-measured 2026-08-19 against `530ccf7dd`: half of the original analysis has
evaporated, and the surviving half is a different bug than the title suggested.**
The design that replaces this file's original root-cause section is
[ADR-0031](../../docs/adr/0031-supply-quit-ownership-and-cold-source-tapping.md);
read that before starting work. This file is kept only as the open-finding
marker and the measurement log.

## Repro

```raku
sub timeout($source, $timeout) {
    supply {
        whenever $source -> $value {
            state $values++;
            emit $value;
            my $last-values = $values;
            whenever Promise.in($timeout) {
                if $last-values == $values { die "Timed out" }
            }
        }
    }
}

my $test-source = supply {
    for 0.05, 0.10, 0.25 { whenever Promise.in($_) { emit 'badger' } }
}
my $timed-out = timeout($test-source, 0.10);
my @received;
my $died = False;
$timed-out.tap: { @received.push($_) }, quit => { $died = True }
sleep 0.5;
say @received.raku;   # raku: ["badger", "badger"]   mutsu: ["badger", "badger", "badger"]
say "died=$died";     # raku: True                   mutsu: False
```

No virtual scheduler needed — this is plain real time. (Test::Scheduler is just
the dist that surfaced it.)

## What changed since the original filing

- **The marker leak is gone.** The originally-recorded symptom ("six 4-element
  marker arrays" instead of values) does not reproduce. `.list` on the same
  shape also returns `("badger", "badger")`, matching raku. The value half was
  fixed by `normalize_promise_whenever_markers` plus the chained-real-tap branch
  at `src/runtime/native_supply_mut_methods.rs:739`.
- **What is left in this repro is quit propagation**, not value delivery: the
  `die "Timed out"` never reaches the tap's `quit =>` handler, so the supply is
  never torn down and a third `'badger'` arrives.

## Surviving root causes (both detailed in ADR-0031)

1. **Quit ownership is attached to the wrong object.** The tap's `quit =>`
   handler is registered per *upstream source* (b1 at
   `native_supply_mut_methods.rs:575`, b2 at `:679`), not once on the supply
   block's own emitter — and the chained on-demand branch (b3, `:739`) registers
   it nowhere at all. Separately, `run_whenever_with_value`'s `ValueView::Promise`
   arm (`src/runtime/subtest.rs:596-611`) discards the body's `Result`, so a
   `die` in a nested `whenever <Promise>` body vanishes. Minimal probes:
   `tmp/probe3.raku` cases B and C in the ADR (both `died=False` in mutsu,
   `died=True` in raku).
2. **`supply_get_values` still replays a cold source instead of tapping it**
   (`src/runtime/supply_promise.rs:239`, with `replay_cold_whenever_capture`
   `:676` and `replay_static_whenever_promise` `:789`). It drops any live inner
   subscription (`if is_live { continue; }`, `:328-330`), so
   `supply { whenever <cold supply whose own whenever is on a live Supplier> }`
   `.list`s to `()` where raku gives the values. This affects only the
   `supply_get_values` family (~20 combinator/`.list`/`.wait` call sites); the
   `.tap`/`.act` and react/`.Promise` paths already chain real taps.

## Affected files

- `src/runtime/supply_promise.rs` — `call_supply_tap`, `supply_get_values`,
  `replay_cold_whenever_capture`, `replay_static_whenever_promise`.
- `src/runtime/native_supply_mut_methods.rs` — the `"tap" | "act"` arm's four
  whenever-source branches and their quit registrations.
- `src/runtime/subtest.rs` — `run_whenever_with_value` (marker construction and
  the Promise arm).
- `src/runtime/native_supplier_methods.rs` — the emit-dispatch error routing
  (`:107-151`) and the canonical `Supplier."quit"` (`:488`).

## Why it is large

Quit ownership touches every whenever-source branch plus the emit-dispatch
error path, and it must not disturb the `QUIT`-phaser protocol
(`take_supplier_whenever_quit_callbacks` / `QuitOutcome`). Replacing the replay
family changes ~20 call sites from a synchronous pull to a bounded drain, with a
real hang risk when the producer runs on the calling thread. ADR-0031 slices
both.

## Impact

`Test::Scheduler` (`TODO_dist` T-037), the last known blocker for that dist:
`t/synopsis.rakutest` and `t/virtualized-time.rakutest` both depend on a
`whenever` body's `die` quitting the enclosing supply. Cro's error paths and
body coercions ride on the same two mechanisms.
