# `call_compiled_closure`'s missing `is rw`/LazyList tail: closed as a static-audit finding with no live bug

A code-inspection audit comparing the tree-walk closure-call branch in `call_sub_value`
(`src/runtime/resolution_call_sub.rs`) against `call_compiled_closure`/
`call_compiled_closure_with_topic` (`src/vm/vm_closure_dispatch.rs`) found that the
tree-walk branch does two things to a closure's return value before handing it back that
the compiled path does not do anywhere: rebuilding a returned `LazyList`'s env with
`__mutsu_preserve_lazy_on_array_assign` set, and calling
`self.maybe_fetch_rw_proxy(v, data.is_rw && !data.is_raw)` to wrap the value in an rw proxy
when the closure/sub is declared `is rw`. `call_compiled_closure_with_topic` returns
straight from `finalize_return_with_spec` with no equivalent tail — confirmed by grepping
`vm_closure_dispatch.rs` for `fetch_rw`/`is_rw`/`LazyList` and finding nothing. This gap
already affects the existing `data.compiled_routine.is_some()` fork in `call_sub_value`,
which jumps straight to `call_compiled_closure` and returns immediately, bypassing the
tree-walk tail entirely.

## Investigation: real, but unreachable

Across three sessions and roughly 15 distinct call shapes, gdb-verified with unconditional
`rust-gdb -batch` breakpoints at the fork site (not just black-box output comparison), no
call shape reachable from ordinary Raku syntax was found to actually hit this gap:

- Every shape reachable from ordinary call syntax (`&f()`, `(&g)()`, `.()`, hash/array-element
  calls, `.wrap`, `.assuming`, map/grep/sort callbacks) never reaches `call_sub_value`'s
  tree-walk tail at all — each resolves through a VM opcode (`CallOnValue`, `CallOnCodeVar`)
  or `vm_call_on_value`'s own dispatch, which apply their own rw-proxy handling
  independently.
- The one confirmed-reachable path, `Promise.then(&named-is-rw-sub)`, does land in the fork
  (backtrace-confirmed via `resolution_call_sub.rs:439`, called from
  `promise_chain_method`'s `on_resolve` callback in `methods_promise.rs`) — but no caller in
  that family (`Promise.then`/`.on_resolve`, Supply-tap callbacks) exposes the callback's
  return value as something Raku code can assign through. `raku` itself rejects
  `$p2.result = 42` with `Cannot modify an immutable Int`, so there is no correct behavior
  for the missing rw-proxy tail to have produced even if it ran.
- The `LazyList` half of the tail turned out to be redundant regardless of call path: the
  `__mutsu_preserve_lazy_on_array_assign` marker is set once, on the `LazyList`'s own `env`,
  at the point `lazy` is evaluated (`dispatch_core_str.rs`), and survives every dispatch
  path simply by traveling with the cloned `Value` — it does not need re-inserting per call.

## Verification (2026-08-21) and closure

Spot-checked two of the specific shapes from the final investigation update against a
fresh `target/debug/mutsu` build:

```raku
my $x = 1;
my &f = sub () is rw { $x }
&f() = 42;
say $x;   # 42 in both raku and mutsu
```

```raku
my $x = 1;
sub f() is rw { $x }
my &g = &f;
(&g)() = 7;
say $x;   # 7 in both raku and mutsu
```

Both still match `raku` correctly. This closes the ticket: the gap between
`call_compiled_closure` and the tree-walk tail is real as a code-inspection observation
and still exists in the code today, but no live/observable bug currently depends on it.
If a future caller of `call_sub_value` is added that exposes a named `is rw` routine's
return value as a user-assignable lvalue (or a returned `LazyList` needs the preserve
marker set fresh rather than inherited), the fix direction described in the original
investigation — adding the tail either as a wrapper at the `call_compiled_closure` call
sites or inlined into `call_compiled_closure` itself — is still the right one to reach for.
