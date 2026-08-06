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
   `def_module_single_sig_body_ok_ignoring_state`. After C6e-2a/2b/2c that is
   ONLY a def with a NativeCall marshalling trait (`is encoded(...)`) — the
   sigilless-scalar, sub-signature and `start`-body exclusions are all lifted
   and the body no longer gates compilation at all. That arm executes
   `eval_block_value_with_pre_post(&def.body)`; with an empty body those defs
   simply stop working.
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

## C6e-2 progress

The residual-arm measurement ran 2026-08-05 (env-gated instrumentation on the
else arm, full `t/` + roast whitelist): 168 hits across `t/` (63 sigilless /
101 `start`-body / 1 sub-signature / 0 trait), 3,677 across roast (~990
sigilless / 2,659 `start`-body / 14 sub-signature / 0 trait). The
`start`-body volume is concentrated in a handful of recursive-start subs
(`conc-fib`, `does-fail`, `fib`).

- **C6e-2a (landed): sigilless scalars.** The gate accepts `\x` scalars;
  the compiled return path flushes the final slot value through the
  `__mutsu_sigilless_alias::` chain before the caller-env merge and
  re-applies the (target, value) pairs unconditionally after it (the merge's
  callee-local exclusion dropped the writeback when the caller variable's
  bare name collided with the param name) —
  `news/2026-08/sigilless-params-run-compiled.md`,
  pinned by `t/sigilless-param-compiled-writeback.t`. The rerouting also
  exposed (and fixed) a pre-existing general bug: a `take` inside a routine
  CALLED from a lazily-pulled gather body corrupted the suspension coroutine
  (`news/2026-08/gather-take-in-callee-eager.md`; residual do-for wrongness
  in `todo/tickets/do-for-over-lazy-gather-drops-first-value.md`).
- **C6e-2b (landed): non-capture sub-signature params** (15 hits total:
  `group-of`, `typed`). The widened-gate A/B (full `t/` + all 37 group-of
  roast files) showed zero real regressions: binding runs through the shared
  `bind_function_args_values` on both arms and destructured elements bind
  read-only, so the compiled binder already reproduced destructuring and the
  slice reduced to removing the exclusion —
  `news/2026-08/subsig-params-run-compiled.md`, pinned by
  `t/subsig-param-compiled.t`. Parameter shapes no longer gate compilation;
  the param predicate is down to NativeCall marshalling traits.
- **C6e-2c (landed): `start`-containing bodies.** The gate excluded ALL
  `start` bodies because a *recursive* sub whose start closure captures a
  param used to break under OTF (the recursive call re-bound the param name
  in the thread env the closure keeps reading — t/start-block-return-value.t
  test 3). The anticipated per-invocation-isolation work turned out to be
  unnecessary: the compiled caller-env merge already excludes the callee's
  own params (`routine_writeback_excluded_names`), so the clobber no longer
  reproduces. Verified by A/B (env-gated widened gate): full `t/` (27,515
  tests) plus all whitelisted S17/S07-hyperrace/integration roast files (218
  files, 3,004 tests) — zero failures — and gdb confirmed the widened gate
  really moves `conc-fib` off the interpreter arm. Since `start` was the
  only leaf that returned true, the whole `module_otf_*_needs_interpreter`
  predicate family and the `RoutineBodyFacts::module_otf_needs_interpreter`
  field are deleted; `def_module_single_sig_body_ok_ignoring_state` is down
  to the NativeCall-trait check. Pinned by `t/start-body-param-compiled.t` —
  `news/2026-08/start-bodies-run-compiled.md`.

## C6e-3 progress

- **C6e-3a (landed): fingerprints survive the drop; body-less code paths
  hardened.** The plan records `body_fingerprint` (structural, over params +
  effective param defs + body) and `RoutineBodyFacts::registration_identity`
  (line-insensitive) at lowering; registration seeds `body_fp_cache` /
  `body_facts_cache` from them, with debug asserts pinning seed == lazy value
  (validated over the whole `t/` suite). Every raw
  `function_body_fingerprint(&def...)` recompute on def-shaped values now
  reads the memoized cache instead (dispatch_proto_call, the fallback's
  candidate filter, hidden-from-USAGE both sides). The forward-declaration
  no-op check reads `metadata.body_is_empty`, not the AST. Sub values built
  from installed defs carry the plan bytecode (`vm_call_named_inner`'s
  sub-decl-as-last-statement, the `$r` trait_mod argument, the
  `is_method_value_decl` `&name` value, the block-lexical escape hatch), and
  the body-classifying fast paths route body-less routine Subs
  (`data.body.is_empty() && data.compiled_routine.is_some()`) to the real
  call path: map/grep/first batchers, sequence generators + Code endpoints,
  `Lock.protect` (runs the routine's own bytecode in the current env —
  File::Temp's END cleanup), the test-assertion callables (`dies-ok &f`),
  and `.yada` (answers from `def.is_stub`).
- **C6e-3b (landed): the safe-class empty body is the default.** The
  C6e-3a `MUTSU_DROP_LEGACY_BODY=1` instrument's predicate is now the
  unconditional registration behavior (the env var is gone): a plan-derived
  def whose plan bytecode resolves for every declared signature registers
  with an empty body. Validated as the instrument configuration in C6e-3a
  (full `t/` 27,519, full `make roast`, battery gate — all green in both
  modes) and re-validated after the flip. Def classes that KEEP their body
  (the C6e-3c cut-line): a plan without resolvable bytecode for every
  signature (class-walker method bodies' nested subs — the predicate checks
  `plan_compiled(0).is_some()`, not just key count), scalar `is rw`/`is
  raw` params (the interpreter-carrier rw relay — RESOLVED, see below),
  routine-level `is rw`/`is raw` and tail `return-rw` (the lvalue machinery
  extracts the assign target from the AST), and NativeCall traits.
- **Scalar rw/raw keep-class LIFTED (2026-08-06):** scalar `is rw`/`is
  raw` params now bind shared `ContainerRef` cells chained to the caller's
  container (`news/2026-08/rw-params-bind-shared-cells.md`), the C6d-4
  gate in `resolution_call_sub.rs` is deleted — rw routines run their
  compiled bodies through `call_sub_value` — and the registration
  predicate no longer checks `has_rw_scalar_param`
  (`news/2026-08/rw-param-routines-register-body-less.md`): rw-param
  routines register body-less like any safe-class def.
- **Signature alternates register with per-slot metadata (2026-08-06):**
  the plan now lowers a `CompiledRoutineMetadata` per `signature_alternates`
  slot (`alternate_metadata`, index-aligned) and
  `register_sub_alternate_decl` seeds the alternate def's
  fingerprint/facts caches from it, exactly like the primary — so an
  alternate candidate's identity no longer depends on a lazy walk over
  the (possibly already empty) plan body
  (`news/2026-08/sig-alternates-register-with-metadata.md`). The C6e-3a
  debug asserts cover the per-slot values wherever a body is still
  attached.
- **Lvalue keep-class LIFTED (2026-08-06):** the plan records the
  assign-target tail at lowering (`CompiledRoutineMetadata::rw_tail_expr`),
  registration seeds `FunctionDef::rw_tail_expr`, and the assign machinery
  prefers it over the body walk (body-less code objects delegate to the
  named path) — so routine-level `is rw`/`is raw`/tail-`return-rw` routines
  register body-less (`news/2026-08/lvalue-tail-from-plan-metadata.md`).
- **C6e-3c (open):** the field itself. `CompiledSubDeclPlan::legacy_body`
  still carries the AST for the remaining keep-classes (unresolvable plan
  bytecode — class-walker nested subs — and NativeCall marshalling traits,
  measured non-vendorable) and for the registration fallback; dropping it
  outright still needs a story for those two. Registration-time body use
  for the safe class is already zero.

Related: `todo/deep/c6d-interpreter-body-sites-are-mostly-token-bodies.md`
(the site inventory), `news/2026-08/fallback-def-arm-runs-compiled-body.md`
(the C6d-5 gate).
