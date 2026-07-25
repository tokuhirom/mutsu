# `await` on deeply-nested iterables no longer drops a Promise

`await` must suspend for *every* Promise it is handed, no matter how deeply that
Promise is nested inside list literals. The sink-context form

```raku
my atomicint $x;
sub p { start { sleep .3; $x⚛++ } }
await (((p(), (p(), (p(),))), (p(), p(), p())), (p(), p(), p()));
is-deeply $x, 9, '&await awaits in sink context, with nested iterables';
```

(from `roast/S17-promise/nonblocking-await.t` test 28) intermittently read
`$x < 9` under load: `builtin_await` descended only two list levels, so a
Promise nested three or more lists deep was pushed through as a raw value and
never `.wait()`ed — the increment it carried had not necessarily landed when
`await` returned. `raku` is deterministically correct; the flat `await @nine`
form never dropped, which localized the bug to the nested-argument walk.

The fix replaces the two hand-unrolled list levels in `builtin_await`
(`src/runtime/builtins_system_async.rs`) with `await_collect_targets`, a
recursive descent that flattens arbitrarily-nested list structure into a flat
list of leaf targets (normalizing each `Supply` to its `.Promise`). A
Promise/Channel is treated as a leaf and never descended, and non-list scalar
values pass through unchanged, so the result-list and Slip-flattening semantics
are preserved.

A deterministic reproduction — a slow Promise nested three list-levels deep
awaited alongside an instant one — went from `1` to the correct `2`. Pinned by
`t/await-nested-iterable.t`.
