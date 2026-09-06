# An imported module sub no longer re-resolves itself on every call

Calling a sub imported from a module cost **14.7 us**; calling an identical sub
declared in the calling file cost **0.75 us**. Same signature, same body, same
call site shape — a 20x penalty for nothing but where the sub was declared.

## Three registry walks per call

`exec_call_func_op` keeps a name-keyed `otf_call_cache` so a repeat call to a
sub that is not in the caller's `compiled_fns` table can dispatch straight to
its compiled body. `compile_and_call_function_def` populates that cache — but
only when it had to compile the body itself:

```rust
let (cf, compiled_from_plan) = match &def.compiled {
    Some(compiled) => (Arc::clone(compiled), true),
    None => (self.otf_compile_function_def(def), false),
};
if !compiled_from_plan && !self.has_multi_candidates_cached(&name) { /* cache it */ }
```

A sub imported from a module always arrives with `def.compiled` already set —
its module was compiled when it was loaded — so `compiled_from_plan` was always
true and the entry was never written. Every single call then re-ran the whole
resolution chain from scratch:

1. `find_compiled_function` — two `Vec<String>` type-signature allocations, a
   cache-key clone, a full `resolve_function_with_types`, then four `format!`ed
   key probes that all miss, because the sub is not in the caller's table at all;
2. `user_function_matches_call` — a second full resolve;
3. `resolve_function_with_types` — a third.

Measured on 20 000 calls: **60 000 `function-full-resolve`s for 20 000 calls**,
against **1** for the same sub declared locally.

## The fix

Cache the plan-compiled body on exactly the same terms as the OTF-compiled one.
The guards that make the OTF entry sound are not specific to who compiled the
body: the entry is keyed by the *callsite* package, invalidated by
`fn_resolve_gen`, and never written for a multi name (whose winner depends on
argument types). A cache hit now also runs the body against the routine's own
nested-sub table (`cf.compiled_fns`) rather than the caller's, which is what the
uncached path next to it already did (ADR-0019 C6e-3c) and what a body compiled
inside another module needs to resolve its own nested `RegisterSub` keys.

| call | before | after |
| --- | --- | --- |
| imported `sub plainmod($c, $d)` | 294 ms / 20 000 | **18 ms** |
| imported `sub modsub(Bool(Mu) $c, $d is copy, $p = '')` | 643 ms / 20 000 | **373 ms** |
| local `sub localsub($c, $d)` (control) | 15 ms / 20 000 | 15 ms |

The simple imported sub is now at parity with a local one. The heavy-signature
one still pays for its own signature — a coercion type, an `is copy`, and a
default keep it off both light call paths and on the full binder — but no longer
pays for its provenance on top.

## Where it showed up

`todo/deep/vendor-real-test-module.md`: under the vendored upstream `Test`,
every assertion calls `proclaim`, which is imported and has exactly that heavy
signature. It was doing three full registry walks per assertion — 280 110 of
`roast/S03-buf/write-int.t`'s 565 212 resolutions. That file's real-`Test` run
drops from 48.2 s to 45.4 s and `proclaim`'s resolve count from 3-per-call to 3
in total; the *rest* of that file's cost is elsewhere (see the ticket), so this
is a 6% slice of it and a 16x win on the general shape.

## The guard the change needed

`t/sub-rw-writeback-attr-source-no-leak.t` was written against a shape that
"does not currently reproduce" through a plain sub call, and said so explicitly:

> If a future change to the sub-call fast paths starts populating `rw_bindings`
> for this shape, `apply_rw_bindings_to_env` would need the same
> attribute-twigil guard `vm_method_dispatch.rs`'s `rw_writeback` loop already
> has — this test would catch the regression either way.

It did, on the first full `make test` after the change. `f(:%!plugin-config)`
encodes the *attribute's* twigil form as the writeback's source name; inserted
verbatim into the caller's env it becomes a pseudo-key an unrelated later method
call's `reconcile_attrs` scan can adopt as its own attribute override, silently
overwriting a different instance's same-named attribute.
`apply_rw_bindings_to_env` now skips an attribute-shaped source, exactly as the
method-call twin does — the shared `ContainerRef` cell the writeback exists for
already carries content mutations without the insert.
