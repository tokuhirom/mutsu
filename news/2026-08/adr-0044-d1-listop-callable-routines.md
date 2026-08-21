# ADR-0044 D1: core listops are callable routines, coexisting with user/imported multis

`push`, `pop`, `shift`, `unshift`, `append`, `prepend`, and `splice` were
purely a compile-time syntactic rewrite (`Compiler::compile_expr_call_inner`
rewriting `push(@a, $v)` into `@a.push($v)` at compile time, emitting
`CallMethodMut`/`ArrayPush`). There was no callable `Sub` object behind any of
the seven names at all, which produced three distinct bugs, all now fixed:

1. **A local `multi` for one of these names destroyed the core candidate
   instead of adding to it.** `multi splice(Str $s, Int $i) { ... }` in scope
   made `splice(@a, 1, 2)` die with `No matching candidates for proto sub:
   splice`, even though raku's real multi-dispatch keeps both the core array
   form and the new candidate reachable.
2. **The same thing happened for an imported `multi`** (`use Module;`
   exporting `multi splice(...) is export` with no accompanying `proto`):
   the import won the name outright and the core array form died.
3. **The seven names were not real callable values.** `&splice(@a, 1, 2)`
   raised `Unknown function: splice`, and — worse — `&push(@a, 7)` and
   `my &f = &push; f(@a, 7)` **succeeded and silently did nothing**, while
   `.defined` and `.^name` reported the value as a genuine `Sub`.

[ADR-0044](../../docs/adr/0044-listops-are-routines-not-a-syntactic-rewrite.md)
records the design (D1/D2/D3). D1 — the slice landed here — gives the seven
listops a native *function*-form implementation
(`src/runtime/listop_functions.rs`, `try_call_listop_function`), reachable
from the existing `dispatch_func_call_inner` -> `call_function_fallback`
chain that already lets a user `multi abs(Str)` extend core `abs`. The
listops were the only builtins missing a function-form candidate for that
chain to fall back to; supplying it was the entire gap (ADR §3).

The implementation deliberately does not reimplement array mutation: it
delegates to the already-correct `call_method_mut_with_values` (the mut-path
method dispatcher), using the call site's own source variable name when
`pending_call_arg_sources` has one (so typed-array element checks,
shared/thread-array bookkeeping, and container-ref cells behave exactly like
the compiled `CallMethodMut` fast path), or a synthetic temp binding
otherwise — mutation is still visible to the caller because a real `Array`
argument's `Gc<ArrayData>` is shared by identity
(`crate::value::aliased_mut::gc_data_mut`), not copied.

A long-standing bug surfaced along the way: `call_function`'s
`"push" | "unshift" | "append" | "prepend"` arm (reached only via `&push(...)`
/ a captured routine value, `call_sub_value` -> `call_function`) unconditionally
returned `Ok(Value::NIL)` for any non-empty argument list — the silent no-op
behind bug 3 above. It now routes to the real fallback via
`call_function_fallback` instead.

D2 (demoting the compiler's `CallMethodMut`/`ArrayPush` rewrite to a pure fast
path, taken only when no competing candidate is visible) needed no code
change: the existing `Compiler::user_listop_shadows` /
`is_imported_function` veto already had exactly that precondition — it was
only the *target* of the veto (an unreachable core candidate) that was wrong.
Pinned by `tests/adr0044_listop_fast_path.rs`
(`push(@a, 1)` still compiles to `ArrayPush` with no competing candidate).

D3 (the accessor/subscript first-argument shapes — `push(@a[2], ...)`,
`push($obj.attr, ...)` — under a competing user `multi`) remains an accepted,
recorded non-goal pending ADR-0036/ADR-0040's element-container work; that
combination now fails loudly (a method-dispatch error) rather than silently
misbehaving.

New regression tests: `t/listop-multi-extends-core.t`,
`t/listop-imported-multi-extends-core.t` (with fixture
`t/lib/ListopMultiExtendsCore.rakumod`), `t/listop-as-code-value.t`. All
three t/ files were verified against real `raku` output byte-for-byte.
