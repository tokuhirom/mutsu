# `.map`/`.grep`/`.first` now invoke a role-mixed `Sub` (`&foo but R`)

`dispatch_map_method`'s `is_callable` pre-check
(`src/runtime/methods_dispatch_match2.rs`) matched `func.view()` directly
against `ValueView::Sub(_) | ValueView::Routine { .. } | ...`. A `Sub` mixed
with a role (`&foo but R1`, `&foo.^mixin(R1)`, or a routine composed via a
`trait_mod:<is>` handler's `$r does Role`) is a `ValueView::Mixin` wrapping a
`Sub`, so it failed the check and `.map()` threw `X::Cannot::Map`:

```
$ mutsu -e 'role R1 { method zz(--> True) {} }; sub double($x) { $x * 2 };
            (1,2,3).map(&double but R1)'
Cannot map a Sub to a Seq, it's not callable.
```

Widening `is_callable` to look through the `Mixin` wrapper got past that
check, but the actual per-element invocation then failed differently
(`Callable expected`) — a second, independent bug. `.grep` and `.first` had
the same two-part shape in their own respective code paths (an
`eval_grep_over_items_with_mutated` compile-once fast path requiring a bare
`Sub`, and TWO independent `.first` implementations — the interpreter's
`InterpFirstMatcher` and a separate VM-native fast path's `VmFirstMatcher` in
`vm_native_first.rs` — each with their own `ValueView::Sub(_)` check).

## Fix

Added `Interpreter::unwrap_callable_mixin` (`src/runtime/resolution_call_sub.rs`),
a single shared helper that looks through a `Mixin` wrapping a
`Sub`/`Routine`/`WeakSub`, matching how `mixin_iteration_target` already
unwraps a Mixin-wrapped collection for `.map`/`.grep` iteration. Wired into:

- `call_sub_value` — the actual invocation, fixing "Callable expected" for
  every caller (not just `.map`/`.grep`/`.first`).
- `dispatch_map_method`'s `is_callable` pre-check.
- `eval_grep_over_items_with_mutated`'s compile-once fast path.
- `find_first_match_over_items` (interpreter `.first`).
- `try_native_first` and `try_lazy_gather_first` (the VM-native `.first`
  fast paths).

New test: `t/map-rejects-role-mixed-sub-as-callable.t`, covering `.map`,
`.grep`, `.first`, a lazy infinite-Range `.map`, and that the mixed-in role
method itself is still callable on the wrapped `Sub`.
