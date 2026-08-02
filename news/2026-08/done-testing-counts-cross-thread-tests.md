# `done-testing` counts tests that ran on other threads

`done-testing` emits `1..N` for the number of tests that actually ran. It read
that number from `TestState::ran` — the *local* counter. Once a test has run on a
spawned thread (a `start` block, a Promise or Supply callback) the authoritative
count lives in the shared atomic `TestState::shared_ran`, and the main thread's
`ran` is stale: `next_ran` bumps the atomic but only updates the running
thread's cloned `ran` field. `TestState::effective_ran()` exists for exactly this
and was already used by the trailing summary — `done-testing` and its
plan-matches check were simply not using it.

So a file whose assertions happen inside supply taps emitted a plan for the
handful of tests the main thread happened to run itself:

```
ok 1 - HTTP2 frame parser is a transform
...
ok 26 - SETTINGS frame with zero content is emitted correctly
1..3
# You planned 3 test, but ran 26
```

Note the summary line already had it right at 26 — only the plan was wrong. The
file failed on the plan alone, with every one of its 26 assertions passing.

That is upstream Cro's `t/http2-frame-parser.rakutest`, whose every
`test-example` / `test-dying` assertion runs inside a `.tap` callback driven from
a `start` block. With this fix (and the supply/whenever lexical-scoping fixes
that got its assertions passing in the first place) the file is fully green.

Pin: `t/done-testing-counts-cross-thread.t`.
