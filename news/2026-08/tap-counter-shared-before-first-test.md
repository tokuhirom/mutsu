# TAP counter is shared with threads spawned before the first test call

A thread spawned BEFORE the first test call never shared the TAP counter:
`TapState::clone_for_thread` shares the counter of an *existing* `TestState`,
but the state is created lazily by the first `plan`/`ok`. When the very first
assertion of a program runs on a spawned thread — the Cro HTTP/2
serializer/parser test shape, where every check of the first `test()` call
fires inside a supply tap driven by a `start` block — the child lazily created
a *private* `TestState`. Its increments never reached the parent, the parent
then restarted numbering at `ok 1`, and prove failed the whole file with
"Tests out of sequence" even when every assertion passed.

Fix: `clone_for_thread_excluding` (`src/runtime/runtime_thread.rs`) now
pre-creates the parent `TestState` before cloning the TAP state, gated on the
Test module actually being loaded (an unconditional empty state would flip
`test_mode_active` and change bare-word resolution for non-Test programs that
spawn threads).

Impact measured on the vendored Cro::HTTP suite: all three HTTP/2
serializer/parser test files previously failed prove with ~26-55 TAP syntax
errors each from the desynced numbering; with the fix,
`http2-request-serializer.rakutest` passes completely (32 tests), and
`http2-response-serializer.rakutest` / `http2-request-parser.rakutest` are
down to exactly one real assertion failure each (tracked in
`todo/deep/closure-read-only-capture-loses-to-caller-env-same-name.md`).

Pin: `t/test-counter-spawn-before-first-test.t` (raku-validated).

Known remaining hole in the same family (intentionally out of scope here): the
child's `failed` count still does not propagate back to the parent, so a
failure that happens only on a spawned thread is visible in the TAP stream
(`not ok`) but not in the parent's plan summary / exit code.
