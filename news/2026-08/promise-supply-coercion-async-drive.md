# `Promise(supply { ... })` no longer blocks the calling thread — fixes a Cro producer-thread deadlock

`Promise(supply { ... })` for an on-demand `supply {}` block previously drove
the react loop that resolves the coercion **synchronously, on the calling
thread**: the coercion did not return until the supply's underlying `done`
fired or a 30-second deadline elapsed. Raku's own semantics are that this
coercion is non-blocking — it returns a `Planned` promise immediately, and
only `await`ing that promise blocks.

The blocking behavior deadlocked whenever the calling thread was also the
thread that had to go on to complete the supply. Cro's response body plumbing
hits this shape directly: `Cro::MessageWithBody`'s `body-blob` is
`Promise(supply { whenever self.body-byte-stream {...} })`, and mutsu's tap
callbacks run synchronously on the emitting thread (`.schedule-on` does not
decouple delivery) — so a handler reading `.body-text` from inside the same
tap that must still call `.done()` on the source stream would block forever
waiting for a value only its own un-run continuation could produce.

Fixed per the design settled in the originating ticket (PR #6183, "option
(a), scoped to the final drive only"): `supply_promise_on_demand` still runs
the supply body synchronously (it must,
to register the `whenever` subscriptions), and the fast synchronous
resolution branches (no subscriptions, static/finite replay, an
already-resolved promise) are unchanged — none of those can deadlock. Only
the trailing `drive_react_subscriptions` call, which polls the live
subscriptions until `done` or the 30s deadline, now runs on a
GC-registered background thread (`spawn_gc_helper_thread`) holding a
thread-clone `Interpreter` built via `clone_for_thread_for_block(&on_demand_cb)`
(so the callback's own captured scalars stay off the cross-thread bare-name
lane the same way a `start {}` block's do). The coercion itself returns the
`Planned` promise immediately; `await` on it blocks on the *returned*
promise instead of inside the coercion, which is exactly what breaks the
deadlock cycle while keeping `await`'s observable blocking behavior.

Pinned by `t/promise-supply-coercion-async-drive.t` (both the immediate-return
shape and the same-thread producer/awaiter deadlock shape, verified against
`raku` first). Verified against the ticket's Cro-shaped repros too: a
`Cro::HTTP::ResponseParser` response body terminated by connection close now
resolves instead of hanging.

A related, deeper gap was found and filed separately rather than folded into
this fix: `todo/deep/nested-on-demand-supply-last-phaser-die-does-not-reach-outer-quit.md`
— a `die` raised from a nested on-demand supply's `LAST` phaser does not
reach an outer subscriber's `QUIT` handler, which is why the
`Content-length`-too-short case (raku: body promise `Broken` with
`X::Cro::HTTP::RawBodyParser::ContentLength::TooShort`) still resolves
`Kept` instead. Confirmed pre-existing and independent of this fix (reproduces
identically on `main` with no `Promise` coercion involved at all).
