# `call_compiled_closure` has no `merge_all`-equivalent knob, and its per-instance state lives in a different store than the tree-walk branch's — both block a general `call_sub_value` → `call_compiled_closure` fork

## Status (2026-08-20): re-verified on `main` (`b1a9bb8a5`), and REFRAMED — design now lives in ADR-0055

Both gaps below are structurally intact on `main`. But a re-investigation
sharpened the diagnosis enough that the fix shape this ticket proposed ("give
`call_compiled_closure` a `merge_all` knob") is now explicitly **rejected**.
The design that replaces it is
[`docs/adr/0055-closure-free-vars-resolve-to-their-own-binding.md`](../../docs/adr/0055-closure-free-vars-resolve-to-their-own-binding.md)
(Proposed). Read that ADR before touching this area; the remainder of this file
is kept as the original audit record plus the corrections below.

### What the re-investigation changed

1. **Gap 2's blast radius is much smaller than recorded.** The two stores are
   both *skipped* for `ContainerRef` captures, and ADR-0025 box-on-capture means
   a mutated capture normally IS a cell. A counter closure driven alternately
   through `.()` (compiled store) and `.classify` (tree-walk store) returns the
   correct running count on `main`; gdb confirms both persistence sites fire and
   the `ContainerRef` merge branch is what carries the value. The real hazard
   window is only the *unboxed residue* — ADR-0025 slice 3's remaining skips
   (type/`where`-constrained scalars, `$`-held Array/Hash, Package-valued).

2. **Gap 1 is worse than recorded, and in the other direction.**
   `call_compiled_closure`'s caller-priority default is not merely "different
   from" the tree-walk `merge_all == false` default — it is **observably wrong**,
   and its apparent correctness is an artifact of the `env_dirty` dual store
   (compiled callers keep lexicals in slots, so the chain probe finds nothing).
   Six-line repro, `raku` says `OUTER`, mutsu says `CALLER`:

   ```raku
   sub noop($v) { 1 }
   my $b = "OUTER";
   noop($b);                      # <- load-bearing: vouch refusal (own_call_arg_sources)
   my $f = { $b };
   sub collide() { my $b = "CALLER"; my $g = { $b }; $g.(); $f.() }
   say collide();
   ```

   Delete the `noop($b)` line and mutsu answers `OUTER`. The `my $g = { $b }`
   line forces the caller's `$b` into `env` instead of a slot; without it the
   compiled path accidentally answers `OUTER` while a `merge_all: true` native
   builtin (`.classify`) still answers `CALLER`. `t/proxy-fetch-capture-vs-caller-lexical.t`,
   `t/closure-readonly-freevar-live.t` and `t/closure-capture-instance-cell.t`
   are all green on `main` — this is a new, uncovered member of that family.

3. **`merge_all` also selects the EXIT writeback policy**, not just the entry
   merge (`resolution_call_sub.rs:903-953` vs `:954-1016`). Retiring the
   parameter means unifying both halves.

4. Current scale: **131** single-line `call_sub_value(..., true)` sites across 44
   files, **109** `..., false)` sites.

5. The sibling ticket `todo/tickets/call-compiled-closure-underscore-arg-binding-bug.md`
   has been fixed and retired; `todo/tickets/call-compiled-closure-missing-rw-lazylist-tail.md`
   is still open and independent of this ADR.

**Not a small fix — do not implement piecemeal.** ADR-0055 sequences it as five
slices, and slice 1 (finish ADR-0025's cell coverage) is a hard prerequisite:
closure-wins is only sound once every mutated capture has a cell.

---

## Context (original audit, 2026-08-14)

`todo/deep/eval-block-value-recompiles-every-call.md` (retired 2026-08-20 to
`news/2026-08/eval-block-value-recompiles-every-call.md`) named, as its "larger
fix", making `call_sub_value`'s general `ValueView::Sub(data)` branch (the
~400-line tree-walk closure-call path in `src/runtime/resolution_call_sub.rs`)
prefer `call_compiled_closure(&data, cc, args, fns)` whenever
`data.compiled_code` is `Some`, mirroring the fork that already exists for
`data.compiled_routine`. An audit was run to check whether
`call_compiled_closure` (`src/vm/vm_closure_dispatch.rs`) is a strict superset
of the tree-walk branch's behavior. It is not. This ticket records the two
structural (not just missing-detail) gaps found.

## Gap 1: `merge_all` has no equivalent in `call_compiled_closure`

`call_sub_value(target, args, merge_all: bool)` takes a `merge_all` parameter
that changes how the closure's captured free-var env is merged against the
caller's live env (tree-walk branch, `resolution_call_sub.rs` around line
489-548):

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
(`vm_closure_dispatch.rs` around line 310-372) has a single, fixed policy: it
force-overwrites only `ContainerRef` captures, `self`, and (for non-routine
blocks) the topic `_`/`!`; every other free var uses `entry_or_insert_sym`
(caller-priority) by default, with `cc.authoritative_free_vars`,
`data.authoritative_captures` and `data.owned_captures` separately
force-overwritten afterward (line 402-431). This is structurally the same shape
as the tree-walk branch's `merge_all == true` policy — there is no way to get
the tree-walk branch's `merge_all == false` **default** behavior out of
`call_compiled_closure` today.

The two policies provably diverge when a closure's free-var name collides with
an unrelated, non-authoritative, same-named caller lexical; this is exactly the
scenario the tree-walk branch's own inline comments say the codebase has hit and
fixed before (roast `S14-roles/anonymous.t`,
`integration/99problems-41-to-50.t` P46), so the collision case is not merely
theoretical. See the Status section above for a live repro on `main`.

## Gap 2: two independent, non-communicating per-closure-instance state stores

Per-instance persisted state for a closure that mutates its own captured free
variables across calls is stored in two different maps, both keyed by
`data.id` (the `SubData`'s stable identity):

- Tree-walk branch: `self.closure_env_overrides: HashMap<u64, Env>`
  (`src/runtime/mod.rs:1362`) — a whole-env snapshot, written at
  `resolution_call_sub.rs` line 842-851 (`persist_closure_env`).
- `call_compiled_closure`: `self.closure_captured_state: HashMap<(u64,
  Symbol), Value>` (`src/runtime/mod.rs:1779`), written at
  `vm_closure_dispatch.rs` line 1104-1127 via
  `get_closure_captured_state`/`set_closure_captured_state`.

A single closure instance (one `data.id`) that is invoked through both paths
accumulates two disjoint persistence records that never see each other's
writes. **This dual-store situation already exists today**, independent of
this ticket's proposed fork: `.()` and any other call reaching
`vm_call_on_value`'s existing `data.compiled_code.is_some()` fast path
(`src/vm/vm_dispatch_helpers.rs:549-568`) already use
`closure_captured_state`, while every call still reaching `call_sub_value`'s
tree-walk branch uses `closure_env_overrides`. Routing *more* of
`call_sub_value`'s traffic through `call_compiled_closure` does not introduce
the hazard, but it does widen how often it can bite, and does so unevenly if
the fork ends up conditional (e.g. gated on `!merge_all`) — hence ADR-0055's
rule that the fork must be gated on the *closure instance*
(`compiled_code.is_some()`), never on `merge_all` or on the call site.

## How this was found

Investigating `todo/deep/eval-block-value-recompiles-every-call.md`'s "larger
fix". A dedicated audit agent read both `resolution_call_sub.rs`'s tree-walk
branch and the entirety of `vm_closure_dispatch.rs`, traced `merge_all` call
sites via `grep`, and verified the two `HashMap` definitions independently in
`src/runtime/mod.rs`. Re-verified and reframed 2026-08-20 with `rust-gdb -batch`
breakpoints on both persistence sites and on all three merge branches, plus
`raku`-validated repros under `tmp/`.
