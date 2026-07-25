# A `whenever <Promise>` registered from inside another `whenever` body is not normalised

Found 2026-07-25 in Test::Scheduler (`TODO_dist` T-037), immediately after
#5409 fixed the top-level case.

## Root cause

#5409 made a `whenever <Promise>` source inside a `supply` block work by
rewriting its subscription marker into a stand-in supplier-backed `Supply`
(`normalize_promise_whenever_markers`, `src/runtime/supply_promise.rs`). That
rewrite runs on the markers the supply block's body registered during its
*initial* run, in the `.tap` path (`native_supply_mut_methods.rs`) and the
`await` path.

A `whenever` registered *dynamically from inside another whenever's body* — i.e.
after the initial run, while the block is already tapped — pushes its marker onto
whatever `supply_emit_buffer` frame is live at that moment, which no longer goes
through the rewrite. The marker is then forwarded to the outer tap as an ordinary
emitted value, exactly the pre-#5409 symptom.

## Repro

Test::Scheduler's `timeout` combinator is the natural one:

```raku
sub timeout($source, $timeout) {
    supply {
        whenever $source -> $value {
            state $values++;
            emit $value;
            my $last-values = $values;
            whenever Promise.in($timeout) {       # <-- registered from inside a whenever body
                if $last-values == $values { die "Timed out" }
            }
        }
    }
}
```

A smaller standalone repro has not been minimised yet — do that first.

## Affected files

- `src/runtime/supply_promise.rs` — `normalize_promise_whenever_markers`,
  `arm_pending_promise_whenevers`.
- `src/runtime/native_supply_mut_methods.rs` — the `.tap` consumer that calls
  both.
- `src/runtime/subtest.rs` — `run_whenever_with_value`, which pushes the marker.

## Fix direction

Likely the right place is `run_whenever_with_value` itself, or the point where an
already-tapped supply drains its emit buffer: a marker pushed after the block is
live has to be rewritten *and* armed there rather than by the initial consumer
loop. Mind the same ordering constraint #5409 documents — a supplier keeps no
backlog, so the promise must not be armed before the body is registered as a tap
on the stand-in.

## Impact

`Test::Scheduler` (`TODO_dist` T-037): `t/virtualized-time.rakutest` reaches test
28 of 83 and then hangs; `t/synopsis.rakutest` passes 3 of 9, the other 6
receiving marker arrays instead of the emitted `'badger'` values.
`t/not-time-based.rakutest` is 3/3.
