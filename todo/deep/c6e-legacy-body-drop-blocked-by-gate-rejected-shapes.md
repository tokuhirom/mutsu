# C6e's legacy_body drop is blocked by the gate-rejected interpreter shapes

Scoping note for ADR-0019 C6e ("redeclaration comparison and eager body facts,
then drop the plan field"). The first two thirds landed as C6e-1; the field
drop cannot follow yet, and the reason is structural, not effort.

## What C6e-1 covered

- The two `body_debug_without_setline` Debug-string comparisons in
  `registration_sub.rs` are now one `registration_identity` hash (params,
  param_defs, top-level-`SetLine`-stripped body — line-insensitive exactly as
  before). It is the single place the redeclaration comparison reads
  `def.body`; C6e-3 redirects it to a plan-recorded fingerprint. Note the
  existing plan fingerprint (`sub_registration_fingerprint`) hashes `SetLine`
  markers, so it is NOT a drop-in replacement — either it grows the same
  filter or the plan records this identity separately.
- `RoutineBodyFacts` is now computed at plan lowering
  (`CompiledRoutineMetadata::body_facts`) and seeded into
  `FunctionDef::body_facts_cache` at registration, so a plan-derived def never
  re-walks its body on a lazy miss. Metadata-less callers (prelude /
  forward-declaration walkers) keep the lazy fill until Phase D.

## Why the field cannot be dropped yet

Dropping `CompiledSubDeclPlan::legacy_body` makes every plan-derived
`FunctionDef.body` empty. Three reader classes still need a real body:

1. **The C6d-5 gate-rejected shapes.** `call_function_fallback`'s interpreter
   arm survives (deliberately) for defs that fail
   `def_module_single_sig_body_ok_ignoring_state` — a sigilless-scalar param
   whose caller-alias writeback crosses an EVAL boundary
   (`t/sigilless-params.t`), and the `module_otf_needs_interpreter` body
   constructs (nested `when` control flow that must not escape the routine —
   `is-deeply-junction` — among others). That arm executes
   `eval_block_value_with_pre_post(&def.body)`; with an empty body those defs
   simply stop working. Killing this dependence means making the compiled
   entry reproduce the sigilless-alias writeback
   (`merge_sigilless_alias_writes`) and the interpreter-only control-flow
   semantics — measure the residual arm hits first (instrument the else arm;
   the C6d-5 survey counted 410 for the whole arm before the fold split it).
2. **`body_fingerprint` identity.** `FunctionDef::body_fingerprint()` and the
   multi-candidate registry keys hash the body; an empty body would collide
   every routine. The plan must hand its fingerprint down and seed
   `body_fp_cache` (mind the SetLine question above — the two fingerprints
   have different line-sensitivity and different purposes).
3. **`vm_call_named_inner.rs`'s sub-decl-as-last-statement case** builds a
   `Sub` value from the plan's `legacy_body` directly; it needs the C6c
   treatment (build from the plan's compiled routine instead).

Suggested subdivision when resuming: C6e-2 = measure + kill the reader
classes (1) and (3); C6e-3 = seed fingerprints and drop the field.

Related: `todo/deep/c6d-interpreter-body-sites-are-mostly-token-bodies.md`
(the site inventory), `news/2026-08/fallback-def-arm-runs-compiled-body.md`
(the C6d-5 gate).
