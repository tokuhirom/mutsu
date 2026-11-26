# Log::Async `04-filter` quarantined: the upstream test assumes `start` order

`Log::Async`'s `t/04-filter.rakutest` sometimes failed the battery release gate (#19233). Its test 9 saw `cat alert2` and its test 10 saw `cat alert1`.

The cause is the upstream module and test, not a mutsu race. `Log::Async.log` emits every message from its own `start $.source.emit($m)`. Two consecutive messages are therefore independent pool tasks racing to the same tap. ADR-15967 §3 states that nothing orders the start of two tasks, and its §4.3 and §8 reject the scheduling gates that would order them.

rakudo reproduces the failure with the identical shape: 6 of 200 runs, on a warmed pool under load. It passes on a cold pool only because it runs a burst of `start`s on a single worker. mutsu failed 14 of 240 runs under the same kind of load. Its emit path is slower (about 160 µs against 60 µs), which widens the window. Its semantics are the same as rakudo's.

Following `docs/flaky-test-policy.md` §3, the file is listed in `flaky-tests.txt` as `battery:Log::Async/04-filter.rakutest` with these measurements. The non-determinism is understood and is not a mutsu defect. The gate now re-rolls it up to three times. Any other failure shape in the file still counts as a real failure.
