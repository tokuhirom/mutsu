# `Supply.tap(:tap)`, `Tap.new(&on-close)` and closing a `Tappable`'s tap

Three pieces of rakudo's tap protocol that mutsu skipped, all needed by EEL
0.3's `MatcherSupply does Tappable` (#16888):

- **`.tap(..., :tap(&cb))`** now calls `cb` with the `Tap` before the supply
  produces anything, and it is the same object `.tap` returns. A `supply { }`
  block (or a `.map`/`.grep` stage derived from one) whose tap is closed in
  that callback never runs its body; a list or `Supplier` supply emits all the
  same, as rakudo's does. mutsu builds its `Tap` after registering the
  subscription, so the callback gets a provisional handle that takes the real
  one's attributes afterwards (`src/runtime/supply_tap_hook.rs`).
- **`Tap.new(&on-close)`** keeps the block and `.close` runs it; it used to be
  dropped.
- **`Supply.new($tappable)`**: the tappable's `tap(&emit, &done, &quit, &tap)`
  now receives a `&tap` that passes the tappable's own `Tap` to the caller's
  `:tap` callback, and closing the `Tap` that `.tap` returns closes the
  tappable's too, so a tappable that stops on close (EEL's runner skips
  matching and suppresses `done`) does.

EEL's "immediate cancellation" checks (`t/06-runner.t` #72,
`t/08-supply-match.t` #54) and "cancelling during EOF output suppresses done
callback" (`t/23-runtime-edge-cases.t` #17) now pass.
