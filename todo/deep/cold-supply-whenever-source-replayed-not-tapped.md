# A cold `supply` used as a `whenever` source is replayed synchronously, so its promise subscriptions leak as values

Found 2026-07-25 in Test::Scheduler (`TODO_dist` T-037). Narrowed 2026-07-25
after the *nested* half of it was fixed (a `whenever <Promise>` registered from
inside another whenever's body — pin `t/supply-nested-whenever-promise.t`); this
file now tracks what is left.

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
say @received.raku;   # raku: ["badger", "badger"]   mutsu: six 4-element marker arrays
say "died=$died";     # raku: True                   mutsu: False
```

No virtual scheduler needed — this is plain real time. (Test::Scheduler is just
the dist that surfaced it.)

## Root cause

`$timed-out`'s only `whenever` source is `$test-source`, which is itself an
*on-demand* (cold, supplier-less) supply. mutsu does not tap a cold source: it
**replays it synchronously** and treats whatever the replay collects as emitted
values (`replay_cold_whenever_capture` / `supply_get_values`). Running
`$test-source`'s body registers three `whenever <Promise>` subscriptions, whose
markers land in the replay's emit-buffer frame and are handed to the outer body
— and out to the tap — as ordinary values.

Two things are wrong, and the second is the real one:

1. **`supply_get_values` still recognises a subscription marker only when its
   source is a `Supply`** (`src/runtime/supply_promise.rs`, the `arr[0]` check
   in the marker-expansion loop). #5409 fixed the two consumers that matter for
   a directly-tapped supply — the `.tap` path and the `await` path — but not
   this one. Making it skip a Promise-sourced marker the way it already skips a
   live source would stop the leak, but would then deliver nothing at all.
2. **A cold on-demand supply used as a `whenever` source should be tapped, not
   replayed.** Synchronous replay can only ever see what the body emits during
   its own run; a body whose emissions arrive later (from a promise, a timer, a
   thread) has nothing to replay. That is why the repro's badgers never reach
   `$timed-out` even once the marker leak is plugged.

## Affected files

- `src/runtime/supply_promise.rs` — `supply_get_values`,
  `replay_cold_whenever_capture`, `normalize_promise_whenever_markers`.
- `src/runtime/native_supply_mut_methods.rs` — the `.tap` consumer, which has
  the working supplier-backed branch to model this on
  (`register_supplier_tap(supplier_id, body_cb, …)` plus the serialize-group and
  done-group wiring).
- `src/runtime/subtest.rs` — `run_whenever_with_value`, which builds the marker.

## Why it is large

Giving a cold on-demand source a supplier and tapping it changes when its body
runs (at subscription time, into a live sink) instead of at replay time into a
captured vector. Every existing consumer of the replay path — `.list`, `.wait`,
the combinators, `await` on a supply — currently depends on getting values back
synchronously, and the done/quit completion accounting (`whenever_supplier_count`,
the done-group marker, `on_close_callbacks`) is written around the two existing
shapes. It also has the ordering hazard #5409 documents: a supplier keeps no
backlog, so nothing may emit into the stand-in before its taps are registered.

## Impact

`Test::Scheduler` (`TODO_dist` T-037), the last known blocker for that dist:
`t/synopsis.rakutest` passes 3 of 9 (the other 6 receive marker arrays instead of
`'badger'`), and `t/virtualized-time.rakutest` reaches test 28 of 83 and then
hangs. `t/not-time-based.rakutest` is 3/3.
