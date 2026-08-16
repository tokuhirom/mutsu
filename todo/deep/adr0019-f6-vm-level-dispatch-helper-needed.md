# ADR-0019 F6: remaining `run_instance_method` families need a VM-level dispatch helper, not `call_method_with_values`

**Status update:** the helper this doc calls for is now implemented —
`Interpreter::try_dispatch_compiled_method_direct` (`src/vm/vm_call_method_compiled_direct.rs`) —
and applied to its first site (instance-ops family's accessor-vs-method resolution branch,
`methods_instance_ops.rs:~1307`). See ADR-0019's F6 box, "Progress (VM-level direct-dispatch
helper, ...)", for the verification writeup. The remaining call sites this doc enumerates
(instance-ops's other two sites, new-dispatch's three sites, mut-dispatch's general fallback,
general-call-dispatch's three sites, qualified-dispatch's shared helper) are still open — each
needs its own per-site migration onto the new helper, following the same discipline as every prior
F6 slice (review what the site does with the returned value beyond the resolved method; verify with
`t/` + a relevant roast subset + the battery gate).

## Summary

ADR-0019 Phase F6 (delete the `run_instance_method` compatibility carrier) established a working
per-site migration pattern with the coercion family (PR #6522) and the mut-lvalue family (PR
#6532): swap `self.run_instance_method_at(<site>, ...)` for `self.call_method_with_values(target.clone(),
method, args)`. This works when the call site lives in a leaf function that
`call_method_with_values` itself never calls into.

**That precondition fails for every other scoped F6 family.** An attempt to apply the same pattern
to two of the instance-ops family's three sites (`methods_instance_ops.rs`'s accessor-vs-method
resolution branch and Package/type-object dispatch branch, inside `dispatch_instance_and_fallback`)
caused immediate, reproducible stack overflow (SIGABRT/SIGSEGV) across dozens of `t/` files — not a
subtle correctness bug, unbounded recursion. Root cause: `dispatch_instance_and_fallback` is itself
called from within `call_method_with_values`'s own ~3900-line body
(`methods_call_dispatch.rs:51`-`3987`), so a call back into `call_method_with_values` from inside it
recurses into itself whenever the modern resolver falls through to the same fallback again for the
same `(target, method)` — which it always does here, since `has_user_method` keeps evaluating true.
The attempt was reverted (`methods_instance_ops.rs` is byte-identical to before); see the ADR
design note's "Negative result (instance-ops family, ...)" progress entry for the fuller writeup.

## Which remaining families are blocked by this, confirmed by call-graph inspection

- **instance-ops** (`methods_instance_ops.rs:1308,1661,1706` at the time of the F6 box's original
  scoping) — all three sites live inside `dispatch_instance_and_fallback`, called from
  `call_method_with_values` (`methods_call_dispatch.rs:2867` reaches it via `dispatch_new` ->
  ... — see below) and from `class_dispatch.rs:234`. **Confirmed blocked** (reproduced the
  recursion directly).
- **new-dispatch** (`methods_object_dispatch_new.rs:61,1418,1573` at scoping time — now
  `try_augmented_builtin_new` line ~61 and the two `dispatch_new` sites) — `dispatch_new` is called
  from `call_method_with_values` at `methods_call_dispatch.rs:2867`. `try_augmented_builtin_new` is
  called from inside `dispatch_new` itself (lines ~833/845), so it's reachable too. **Blocked by the
  same call-graph shape** (not yet reproduced with an actual attempt — inferred from the call graph,
  same as the instance-ops sites were before they were tried and confirmed).
- **mut-dispatch** (`methods_mut_dispatch.rs:28,2777`) — both sites are *inside*
  `call_method_mut_with_values` itself (the mut analog of `call_method_with_values`; the function
  spans the whole file from line 11). Calling `call_method_mut_with_values` from within itself is
  self-reference by construction, not merely call-graph-reachable. **Blocked**, same shape as the
  general-call-dispatch family's own already-known self-reference site.
- **general-call-dispatch** (`methods_call_dispatch.rs:70,581,3942`) — all three sites are *inside*
  `call_method_with_values` itself. **Blocked** by construction; this was already known and stated
  in the F6 box text before any attempt.
- **qualified-dispatch** — not blocked by *this* mechanism, but blocked by a different one: its
  sole site now calls `run_resolved_method_compiled_or_treewalk`, a *shared* resolved-path helper
  used at 8+ call sites across `methods_qualified.rs` alone (plus likely more elsewhere). Retiring
  that helper is a separate, larger task than a single-site swap — it is itself part of the
  `run_instance_method` family's "two resolved-path helpers in class_dispatch.rs" the F6 box
  originally named, not yet scoped as its own sub-slice.

Only **coercion** (`types/coercion.rs`) and **mut-lvalue** (`methods_mut_method_lvalue.rs`) turned
out to be leaf call sites outside `call_method_with_values`'s / `call_method_mut_with_values`'s own
call graph — both are now migrated (PRs #6522, #6532). Every other scoped family needs a different
fix shape.

## What the real fix likely looks like

The F6 box's own text already named the direction for the general-call-dispatch family's
self-reference problem: "that site specifically needs the VM-level `resolve_method_cached`/
`dispatch_compiled_method` pair directly" (`vm_call_method_compiled_cache.rs`), not
`call_method_with_values`. This finding generalizes that to every other remaining family too — they
all need the same VM-level direct-dispatch path, which resolves a method and invokes it without
going through the whole `call_method_with_values`/`call_method_mut_with_values` front door (and
therefore without risking re-entering the very fallback that's calling it).

Concretely, this needs:

1. A small helper (or direct use of the existing `resolve_method_cached` +
   `dispatch_compiled_method` pair) callable from *within* `call_method_with_values`,
   `call_method_mut_with_values`, and `dispatch_instance_and_fallback`/`dispatch_new` without
   re-entering the outer dispatch functions.
2. Care around what each site currently gets from `run_instance_method_at` beyond the resolved
   value — e.g. the instance-ops accessor site's `updated: AttrMap` (shown to be redundant once a
   modern-path call commits through the shared cell directly, per the mut-lvalue slice's own
   finding — but that redundancy hasn't been verified for a *non-recursive* modern-path call yet,
   since the only modern-path call tried here recursed before any behavior could be observed).
3. Raku-verified regression coverage per family, same discipline as coercion/mut-lvalue, but this
   time also covering the actual recursion hazard specifically (a test that exercises the exact
   fallback-reentry shape, not just correctness of the resolved value).

## Why this belongs in `todo/deep/`, not a ticket

Building the VM-level direct-dispatch helper touches `vm_call_method_compiled_cache.rs`
(`resolve_method_cached`/`dispatch_compiled_method`), needs verification it behaves identically to
the ad-hoc walker for every one of the ~10 remaining call sites across 4 families, and risks the
same class of correctness bug the E4 sequence-resolver gap already surfaced once (mismatched
candidate-set semantics for type-object/role-pun receivers, fixed in the mut-dispatch family's own
slice). This is real design work, not a mechanical per-site swap.

## Pointers

- ADR-0019 design note, Phase F6 box (`docs/adr/0019-compiled-declarations-and-unified-method-dispatch.md`,
  search "F6 — Delete compatibility call carriers") — the coercion/mut-lvalue progress notes show
  the working pattern; the "Negative result (instance-ops family...)" note documents the repro.
- `src/runtime/methods_call_dispatch.rs` — `call_method_with_values` (the ~3900-line function
  containing 3 of the general-call-dispatch sites plus the `dispatch_new` call at line ~2867).
- `src/runtime/methods_mut_dispatch.rs` — `call_method_mut_with_values` (contains both mut-dispatch
  sites).
- `src/runtime/methods_instance_ops.rs` — `dispatch_instance_and_fallback` (all 3 instance-ops
  sites).
- `src/runtime/methods_object_dispatch_new.rs` — `dispatch_new` / `try_augmented_builtin_new` (all
  3 new-dispatch sites).
- `src/vm/vm_call_method_compiled_cache.rs` — `resolve_method_cached`/`dispatch_compiled_method`,
  the existing VM-level pair the fix should build on.
