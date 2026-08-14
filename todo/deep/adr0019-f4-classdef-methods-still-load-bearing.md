# ADR-0019 F4 scoping: `ClassDef::methods` is still the write-side source of truth, not a dead mirror

F4 ("Remove `ClassDef::methods` as a dispatch/registration mirror. Leave type structure metadata
beside the canonical method table and update snapshots/rollback to copy one source.") was checked
as a candidate next Phase F slice after F5's closure, on the theory that F1/F2's introspection
cutover (`.^methods`/`.^method_table` via #6399/#6400, `.^can`/`.can` via #6402/#6406) might have
already removed the last readers that justified keeping `ClassDef::methods` around (per D2d's note
that it was a "lateral move" to migrate `.^methods`/`.^can` off it back when `class_def.methods` and
`MethodEntry` were "already a single source of truth kept in lockstep by
`sync_user_method_entries`"). A read-only survey (no code changed) found this is not the case.

## What the survey found

1. **The sync direction is the opposite of what F4 wants.** `Registry::sync_user_method_entries`
   (`src/runtime/registry.rs:361`) reads `class_def.methods.clone()` and writes it into
   `MethodEntry.user_candidates` — `ClassDef::methods` is the source, the canonical table is the
   derived copy. F4's framing ("canonical table is the one source") requires reversing this, not
   just deleting a mirror.
2. **Live dispatch-path readers of `class_def.methods` are numerous and current**, not leftover dead
   code: MRO resolution (`resolution_method.rs:597,602,637,642`), `nextsame`/`callsame` deferral
   chain building (`resolution_deferral.rs:52,57`), private method resolution
   (`resolution_private_method.rs:90`), the `run_instance_method` carrier itself
   (`class_dispatch.rs:228` — the same carrier F6 wants to delete), `.^lookup`/MOP
   (`methods_classhow_lookup.rs:57,276,302`, `methods_classhow_dispatch.rs:689,806,862,1263`),
   `accessors_state.rs:560,565,1088`, `methods_walk.rs:658,693`,
   `methods_signature_shaped.rs:300,349`, `methods_qualified.rs:422`, BUILD/TWEAK plan/existence
   checks (`ctor_phase_plan.rs:67,103`, six sites in `methods_object.rs`), and introspection/MOP
   synthesis (`class_introspection.rs:39,242,262,323`, `metamodel.rs:303,342,365,411,428`). F1/F2's
   cutover only touched the introspection surface (`.^methods`/`.^can`), not the dispatch/MOP
   internals above.
3. **Write sites are equally numerous**: `registration.rs:207,209,367,369,636`,
   `registration_class_body_attr.rs`, `registration_class_body_method.rs`,
   `registration_class_compose.rs`, `registration_role_*.rs`, `system.rs`,
   `methods_classhow_dispatch.rs:862`, all inserting/removing directly on `class_def.methods`.

`.methods` hits total 143 across the codebase (excluding `decl_types.rs`'s own field definition);
over 40 files touch a `ClassDef`/`RoleDef` instance's `.methods` for real, non-test reasons.

## What this means

F4 is not a "delete a field plus its now-dead readers" slice. It requires cutting over ~15-20
files' worth of live dispatch/MOP/BUILD-TWEAK entry points to read the canonical `MethodEntry`
table instead, reversing `sync_user_method_entries`'s write direction, and re-verifying every
snapshot/rollback path (class redeclaration, augment rollback, EVAL class restoration) against the
new single source. That is Phase-E-entry-routing-sized work (comparable to E5-E7), not a quick
plumbing change.

## Before picking this up

Needs its own dedicated design pass first, the same way F1/F2/F3 do: classify each read site above
by whether it can shadow-check-then-cutover safely (E1a/E4a style) before any code changes. Do not
attempt this as a single-session slice without that classification done first.
