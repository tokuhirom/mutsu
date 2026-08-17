# `t/supply-done-in-tap-callback-is-not-a-failure.t` test 3's load-flakiness was a test-design race, not a runtime bug

Test 3 was measured failing 10/24 runs under 24-way parallel invocation on a
loaded machine (2026-08-12, Text::CSV campaign), and once in an ordinary
`make test` run. Serial re-runs passed 5/5, so it looked like load/timing
sensitivity rather than a deterministic regression.

## Root cause

The test's third block relays data from a socket-reading `whenever` (running
on its own reader thread) through a `Supplier`, into a *second*, downstream
`whenever` that calls `done`:

```raku
my $upstream = supply {
    whenever $conn.Supply -> $data { $relay.emit($data); }
}
my $up-tap = $upstream.tap(-> $ { });        # starts the upstream reader

my $downstream = supply {
    whenever $relay -> $data { done; }
}
my $down-tap = $downstream.tap(..., done => { $done.keep(True) });  # subscribes to $relay
```

`$up-tap`'s `.tap()` call starts the upstream `whenever` immediately — and
`Supplier.emit` does **not** buffer for late subscribers. Under enough CPU
contention, the reader thread can read the socket and call `$relay.emit(...)`
*before* the downstream `.tap()` a few lines later has finished subscribing,
silently dropping the emit forever. Confirmed with a longer-timeout probe:
under heavy synthetic load (12 `yes` processes pinning all cores), the test
was bimodal — either completing in ~5-15ms or hanging the full length of
whatever timeout was given (60s tested), never something in between. That
rules out "just slower under load" and confirms a genuine dropped event.

Swapping the two `.tap()` calls — subscribing `$downstream` to `$relay`
*before* starting `$upstream`'s reader — closes the window entirely: 40/40
runs passed under the same heavy-load reproduction that previously failed
25/40 (and 0/15 failed with a 60s timeout, vs. bimodal pass/hang before the
reorder). This is also the generally-correct pattern for `Supplier`-based
code (subscribe before a producer can emit), so the fix strengthens the test
rather than merely working around mutsu's scheduling.

No interpreter change was needed — this was a test-design race, not a
`done`/Supply-propagation bug in mutsu's runtime.
