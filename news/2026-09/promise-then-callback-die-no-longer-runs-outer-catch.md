# A dying `.then` callback no longer runs the `CATCH` around the `.then` call

`Cro::Core`'s `t/message-with-body.rakutest` failed now and then with a
`throws-like` subtest that "planned 2 tests, but ran 4"
([#10301](https://github.com/tokuhirom/mutsu/issues/10301)). The test does
`throws-like { await(TestBodyA.new.body-text) }, X::Cro::BodyNotText`, and
`body-text` is `self.body-blob.then: { ... die X::Cro::BodyNotText ... }`.

When the body-blob promise happened to be kept *before* `.then` was called,
mutsu ran the callback synchronously on the calling interpreter. Since
ADR-0072, a `die` runs the active `CATCH` handlers inline, at the throw site —
so the callback's `die` ran `throws-like`'s `CATCH` (two assertions), even
though the failure only breaks the derived promise and never reaches the
caller. `await` then rethrew it and the same `CATCH` ran again. Whether the
promise was already kept depended on thread timing, hence the rare failure.
It reproduces deterministically with `Promise.kept(1).then({ die })`, and
also made a `CATCH` around a bare `.then`/`.andthen`/`.orelse` call fire for
a failure it must never see.

The synchronous callback now runs behind a catch marker (`with_catch_marker`,
the same boundary lazy-list forcing already uses), so the inline handler
chain stops at the promise. The body of a `start` cued on a user scheduler
(`run_cued_start_body`) gets the same boundary, since its failure is likewise
turned into a broken promise.

Two unrelated mismatches found while probing are filed as
[#10311](https://github.com/tokuhirom/mutsu/issues/10311) (`start` ignores a
dynamic `CurrentThreadScheduler`) and
[#10312](https://github.com/tokuhirom/mutsu/issues/10312) (a tap callback's
`die` goes to its `quit` handler instead of the `emit` caller).
