# Sibling threads' distinct `my @a` bindings no longer merge through the root-scoped atomic array/hash lane

The `__mutsu_atomic_arr::`/`__mutsu_atomic_hash::` shared-store lane serializes concurrent
`@a.push`/`%h{k} = v` on a plain lexical. It resolved unconditionally at the process root store —
correct for genuinely process-wide primitives like `atomicint` and scalar `cas`, but this lane
exists to guard exactly ONE lexical binding, not a process-wide name.

A worker thread's own `my @a` normally masks the name in `thread_redeclared_vars`, keeping it
frame-local. But any *nested* spawn inside that worker (a `start` inside a `start`, or — the shape
that surfaced this — `Cro::HTTP::Client`'s per-request `start` inside a caller's own `start`) drops
that mask, by design, so genuinely-shared re-declared names still propagate to further-nested
children. With the mask gone, `push @a` routed through the name-keyed atomic lane, and that lane's
root-scoped resolution meant two completely unrelated sibling threads' own `my @a` bindings both
funneled into the SAME `__mutsu_atomic_arr::@a` entry — each thread's final read then saw the
other's pushes interleaved into its own array.

Fixed by scoping the lane at the lineage that owns the base name's binding, falling back to root
only when no lineage owns it yet: `SharedStore::scope_for` now routes an atomic-lane key through
`owner_of(base_name)`, so every caller going through the standard `get`/`set`/`contains_key`/
`with_entry_mut` API picks up the fix automatically. The dozen call sites in
`builtins_atomic_shared.rs` that lock the atomic store's `own_map()` directly (bypassing
`get`/`set`) were switched to a new `SharedStore::atomic_lane_scope` so they stay in lockstep with
`scope_for`'s routing instead of split-braining against it. Plain scalar `atomicint`/`cas` keys
stay root-scoped, unchanged — they are genuinely process-wide by design.

Verified against the original ticket's isolated repro (was reproducing every run; now stable
across repeated runs) and its negative control (a parent-declared array shared with children still
correctly merges their pushes, so `t/lock.t`-style genuine sharing is untouched). Pinned by
`t/sibling-thread-array-lane-scope.t` (checked against `raku` too). Full local `make test` (28163
tests) and all 99 S17 whitelist roast files (1603 tests) green.

This was diagnosed as the root cause of `http-session-inmemory.rakutest`/`http-session-persistent.rakutest`
subtests 8-9 ("No session confusion with concurrent clients"), and the fix genuinely resolves the
array-merge part of that symptom — both clients' request arrays are now the correct length in the
correct order. However, those two subtests still fail for a **different**, newly-surfaced reason:
concurrent clients' session-counter increments interleave as if they shared one `SessionData`
instance, even though the request arrays no longer merge. That is a distinct bug, tracked
separately in `todo/tickets/concurrent-http-sessions-share-one-instances-count-attribute.md`.
