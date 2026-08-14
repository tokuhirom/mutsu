# `call_compiled_closure` has no `merge_all`-equivalent knob, and its per-instance state lives in a different store than the tree-walk branch's — both block a general `call_sub_value` → `call_compiled_closure` fork

## Context

`todo/deep/eval-block-value-recompiles-every-call.md` names, as its "larger
fix", making `call_sub_value`'s general `ValueView::Sub(data)` branch (the
~400-line tree-walk closure-call path in `src/runtime/resolution_call_sub.rs`)
prefer `call_compiled_closure(&data, cc, args, fns)` whenever
`data.compiled_code` is `Some`, mirroring the fork that already exists for
`data.compiled_routine`. An audit was run to check whether
`call_compiled_closure` (`src/vm/vm_closure_dispatch.rs`) is a strict superset
of the tree-walk branch's behavior. It is not. This ticket records the two
structural (not just missing-detail) gaps found; the two smaller, independently
fixable bugs found in the same audit are filed separately as
`todo/tickets/call-compiled-closure-underscore-arg-binding-bug.md` and
`todo/tickets/call-compiled-closure-missing-rw-lazylist-tail.md`.

## Gap 1: `merge_all` has no equivalent in `call_compiled_closure`

`call_sub_value(target, args, merge_all: bool)` takes a `merge_all` parameter
that changes how the closure's captured free-var env is merged against the
caller's live env (tree-walk branch, `resolution_call_sub.rs` around line
449-514):

- `merge_all == false` (the default, used by most calls into `call_sub_value`):
  for every non-`ContainerRef`/non-`self` free var, the closure's OWN captured
  value unconditionally **overwrites** whatever the caller's env currently
  holds under that name (`new_env.insert_sym(*k, v.clone())`).
- `merge_all == true`: for every free var that is not in the closure's
  `authoritative_free_vars`/`authoritative_captures` (the compiler-proven
  "this closure owns this binding and never sees it mutated after capture"
  set), the CALLER's existing value wins if present
  (`new_env.entry_or_insert(...)`) — used where a native-invoked callback must
  observe live, possibly-mutated state (a `Proxy` FETCH/STORE pair, a
  `Promise` executor, a `unique`/`squish`/`min`/`max` comparator, an atomic
  `cas` code block, etc).

`call_compiled_closure_with_topic`'s own env-merge loop
(`vm_closure_dispatch.rs` around line 310-360) has a single, fixed policy: it
force-overwrites only `ContainerRef` captures, `self`, and (for non-routine
blocks) the topic `_`; every other free var uses `entry_or_insert_sym`
(caller-priority) by default, with `cc.authoritative_free_vars` and
`data.authoritative_captures` separately force-overwritten afterward (line
390-404). This is structurally the same shape as the tree-walk branch's
`merge_all == true` policy — there is no way to get the tree-walk branch's
`merge_all == false` **default** behavior (unconditional closure-value-wins
for every non-authoritative free var) out of `call_compiled_closure` today.

**Grep count:** 97 call sites currently pass `merge_all: true` explicitly
(saved list regenerable via `git grep -n 'call_sub_value(.*, true)' src/`,
spanning `Promise`/`.then`, Supply taps, `unique`/`squish` comparators,
`min`/`max`/reduce comparators, atomic `cas` code blocks, `Proxy` FETCH/STORE
in `builtins_lvalue.rs`, subtest callbacks, scheduler callbacks). These sites'
desired semantics already match `call_compiled_closure`'s default, so routing
them through it is plausible. The remaining, much larger population of
`merge_all == false` call sites is the one at risk: whether
`call_compiled_closure`'s caller-priority-except-authoritative default
produces the *same observable result* as tree-walk's unconditional overwrite
depends on how complete the compiler's `authoritative_free_vars`/
`authoritative_captures` computation is for arbitrary closures reached via
this general branch — which has not been verified. The two policies provably
diverge only when a closure's free-var name collides with an unrelated,
non-authoritative, same-named caller lexical; this is exactly the scenario the
tree-walk branch's own inline comments (`resolution_call_sub.rs` line
449-458) say the codebase has hit and fixed before (roast
`S14-roles/anonymous.t`, `integration/99problems-41-to-50.t` P46), so the
collision case is not merely theoretical.

## Gap 2: two independent, non-communicating per-closure-instance state stores

Per-instance persisted state for a closure that mutates its own captured free
variables across calls is stored in two different maps, both keyed by
`data.id` (the `SubData`'s stable identity):

- Tree-walk branch: `self.closure_env_overrides: HashMap<u64, Env>`
  (`src/runtime/mod.rs:1260`) — a whole-env snapshot, written at
  `resolution_call_sub.rs` line 820-829 (`persist_closure_env`).
- `call_compiled_closure`: `self.closure_captured_state: HashMap<(u64,
  Symbol), Value>` (`src/runtime/mod.rs:1617`) — per-free-var, written at
  `vm_closure_dispatch.rs` line 1064-1083 via
  `get_closure_captured_state`/`set_closure_captured_state`.

A single closure instance (one `data.id`) that is invoked through both paths
accumulates two disjoint persistence records that never see each other's
writes. **This dual-store situation already exists today**, independent of
this ticket's proposed fork: `.()` and any other call reaching
`vm_call_on_value`'s existing `data.compiled_code.is_some()` fast path
(`src/vm/vm_dispatch_helpers.rs` line 549-569) already use
`closure_captured_state`, while every call still reaching `call_sub_value`'s
tree-walk branch uses `closure_env_overrides` — so a closure already invoked
both via `.()` and via a `merge_all`-using native builtin is already exposed
to this split. Routing *more* of `call_sub_value`'s traffic through
`call_compiled_closure` does not introduce the hazard, but it does widen how
often it can bite, and does so unevenly if the fork ends up conditional (e.g.
gated on `!merge_all`) — a `merge_all == true` call and a `merge_all == false`
call on the *same* closure instance would then deterministically split across
stores every time, rather than only occasionally colliding with an unrelated
`.()` call.

## What would need to happen to make the general fork safe

1. Give `call_compiled_closure_with_topic` a `merge_all`-equivalent mode (an
   extra bool parameter, or a fourth env-merge policy alongside the current
   ContainerRef/self/topic special cases) that reproduces tree-walk's
   unconditional-overwrite-for-non-authoritative-free-vars default, and route
   `merge_all == false` calls through it using that mode.
2. Decide whether the two persistence stores should be unified (one map,
   consulted by both call paths) or whether it's acceptable to leave them
   separate as long as a single conditional fork keeps a given closure's calls
   consistently on one path or the other — this needs auditing whether any
   single `SubData` in practice gets called with a mix of `merge_all` values
   across its lifetime (plausible for a generic wrap/callback combinator
   passed to multiple different consumers).
3. Re-run the parity audit against `call_compiled_closure` once (1) lands, to
   confirm the divergence class described in Gap 1 is actually closed and not
   just narrowed.

## How this was found

Investigating `todo/deep/eval-block-value-recompiles-every-call.md`'s "larger
fix". A dedicated audit agent read both `resolution_call_sub.rs`'s tree-walk
branch and the entirety of `vm_closure_dispatch.rs`, traced `merge_all` call
sites via `grep`, and verified the two `HashMap` definitions independently in
`src/runtime/mod.rs`.
