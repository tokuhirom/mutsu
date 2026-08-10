# A bare `when` inside a deferred callback created inside a sub/method no longer leaks `Control::Succeed`

`call_sub_value`'s "bare blocks and pointy blocks are not routine boundaries for `return`" special
case existed to let an explicit `return` inside a closure like `@a.first: { return $_ * 10 if
$_ == 3; False }` reach back to its lexically enclosing routine, by redirecting the error to the
closure's captured `__mutsu_callable_id`. The gate for "does this error need redirecting" was
`e.return_value.is_some()` — but `return_value` is not exclusive to an explicit `return`: a
`when`/`default` succeed signal also carries one, since it needs to pass the matched branch's tail
value up to an enclosing `given`/`for`.

A callback closure created inside a `sub`/`method` — `.tap: { when Int {...} }` registered from
within `sub setup(Supplier $s) {...}`, or from a `method`/`submethod TWEAK` — captures
`__mutsu_callable_id` in its environment so a genuine `return` inside it can target that routine.
When the callback fires later, asynchronously (the supply emits well after `setup()` has already
returned), a matched `when`'s succeed signal was misrouted through this same "redirect to
`__mutsu_callable_id`" path as if it were `setup()`'s own `return`. No live frame with that
callable id existed anymore, so it escaped as an uncaught runtime error and killed the process — a
top-level `.tap: { when ... }` callback (no enclosing routine, so no captured
`__mutsu_callable_id`) never had this problem, because its succeed signal instead reached
`finalize_return_with_spec`'s generic "an error carrying a `return_value` becomes `Ok`" fallback,
which absorbs it correctly.

Fixed by gating the redirect on `e.is_return()` (checks the error's `Control::Return` discriminant)
instead of the broader `e.return_value.is_some()`, so only a genuine `return` control signal is
eligible for the non-local-return path. A live-frame `return` from a closure is unaffected; a
`return` whose target routine has already returned still errors, matching raku's own "Attempt to
return outside of ... Routine" behavior for that case.

Root-caused via `rust-gdb` backtrace comparison between a passing (top-level tap) and failing
(tap registered inside a `sub`) repro that were byte-identical up through the point the succeed
signal originates — the divergence turned out to be several frames further up the unwind, in data
(`data.env.contains_key("__mutsu_callable_id")`) rather than control flow. Pinned by
`t/when-in-deferred-callback-created-inside-sub.t` (verified against `raku`, including that the
genuine-return and target-already-gone cases still behave correctly). Full local `make test`
(28164 tests) and all 99 S17 whitelist roast files (1603 tests) green; Cro's
`http-middleware.rakutest` (24/24, the file this exact mechanism used to abort ~1/3 of runs)
stable across repeats.
