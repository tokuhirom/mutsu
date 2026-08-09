# ADR-0019 D3-8 design: compile method bodies in the main-pass compiler

Design pass for the D3-8 box scoped by the D3-7 session (see the ADR's Phase D section): method
**bodies** are the last routine-body class still compiled at runtime by a throwaway
`Compiler::new()`, unlike `sub` bodies, which Phase C (C1-C8) moved to main-pass compilation with
pool-keyed `CompiledFunction`s. This document records the investigation results, the design
decisions, and a slice plan. No code has landed for this box yet.

## Problem statement

Every method/submethod body is compiled by
`Interpreter::compile_method_def_in_place_with_dist` (`src/runtime/accessors_resolve.rs:17-60`):
a fresh `Compiler::new()` per method, seeded with only four things — the method package
(`original_role`/`role_origin`/class), `current_distribution`, `lexically_in_method = true`, and
the synthetic param prefix `["self", "__ANON_STATE__", "?CLASS", "?ROLE"]` — memoized solely by
`def.compiled_code.is_some()`. Costs, confirmed by survey:

1. **Registration-time compilation, repeated per registration.** A class declared inside a loop
   or a repeatedly-called sub recompiles every method body on every registration (the
   `compiled_code.is_some()` guard only helps within one `MethodDef`'s lifetime, and registration
   builds fresh `MethodDef`s).
2. **Role methods are compiled once per composing class, by accident.** Composition reads the
   `role_candidates` snapshot (`registration_role_decl.rs:286-297`), which is cloned *before*
   `compile_role_methods` runs (`vm_typedecl_ops.rs:672`), so composed defs arrive with
   `compiled_code: None` and each composing class recompiles byte-identical bodies. (The compile
   inputs are composition-invariant: package comes from the role, `T` resolves via env at call
   time, `::?CLASS`/`::?ROLE` bind at method entry — see "Facts" below.)
3. **One dispatch path recompiles per call.** `class_dispatch.rs:497-507` compiles into a local
   `compiled_holder` clone and throws the result away after the call.
4. **The main-pass compiler already compiles every method body once and discards the bytecode.**
   `record_type_body_captures` (`src/compiler/helpers_sub_body.rs:752-786`) runs a full
   `compile_closure_body` per top-level method statement purely to harvest
   `free_var_writes` for `type_body_written_lexicals`, then drops the compiled code.
5. **The throwaway compiler has zero lexical context** — no `inherit_enclosing_scopes`, no
   `inherit_fold_ctx`, no `inherit_outer_code_var_names` — unlike `compile_sub_body`
   (`helpers_sub_body.rs:205-259`). This is why a class declared inside a sub cannot see the
   sub's lexicals (`sub outer($n) { class C { method m { $n * 2 } } }` — mutsu returns 0, raku
   42). D3-8 does not fix that bug (it also needs runtime capture semantics), but the main-pass
   mechanism is the prerequisite for ever fixing it.

The goal mirrors Phase C: compile each method body **once, at main-pass compile time**, key it
into the program's `CompiledFns` table, carry the key on the declaration plan, and have
registration install `MethodDef.compiled_code` by table lookup — with
`compile_method_def_in_place_with_dist` demoted to the fallback role `otf_compile_function_def`
plays for subs (existing fallback narrowed, not a new one).

## Facts the design rests on (survey results, 2026-08-08)

**The Phase C mechanism to mirror** (`sub` side):

- `Stmt::SubDecl` compilation order: plan + `RegisterDecl` emitted first (`stmt.rs:3180-3192`),
  then the body compiled via `compile_sub_body_with_deprecation` (`stmt.rs:3235-3266`), then keys
  attached to the plan via `set_sub_decl_compiled_routine_keys` (`stmt.rs:3283-3284`,
  `opcode.rs:5742-5748`).
- Keys index `CompiledFns = FxHashMap<Symbol, CompiledFunction>` (`opcode.rs:5874`), a side table
  threaded through VM execution; `exec_register_sub_op` resolves
  `compiled_fns.get(&compiled_routine_keys[slot])` with a length invariant that degrades to
  "no bytecode" (runtime fallback) rather than a shifted mapping
  (`vm_register_sub_ops.rs:249-254`).
- Nested compilation units merge child `compiled_functions` into the parent with collision
  renaming (`#import{N}`) plus `remap_sub_decl_compiled_routine_keys` rewriting every plan key,
  recursively through `closure_compiled_codes` (`helpers_sub_body.rs:29-61`,
  `opcode.rs:3249-3270`); the post-remap snapshot also becomes the routine's own
  `CompiledFunction::compiled_fns` so detached values still resolve nested keys (C6e-3c).
- A package-level `method` outside a class is *already* main-pass compiled through this exact
  machinery: lowered to a synthetic `SubDecl` and compiled with the same four synthetic params
  prepended (`stmt.rs:3286-3344`) — the direct precedent that method bodies compile fine through
  `compile_sub_body`-shaped code.

**What the runtime method compile actually depends on** (all of it available or replicable at
main-pass time for the static-name case):

- Package: `original_role`/`role_origin`/class storage name (`accessors_resolve.rs:30-35`). For a
  class walker method this is the qualified class name; for a role walker method the qualified
  role name. Statically known unless the declaration name is computed (`class ::($n)`), which is
  exactly the case D3-1's `method_name_chunks` fallback already models.
- `current_distribution` (`accessors_resolve.rs:36`): the runtime resolves it per class/role via
  `resolve_package_distribution`; at main-pass time the enclosing compiler's
  `current_distribution` is the declaring compunit's distribution — same value for a
  declaration being compiled in its own compunit (verification item V3).
- Effective params: `MethodDef.params`/`param_defs` are the *registration-derived* effective
  set — `effective_method_param_defs` (`registration.rs:60-69`, appends `%_` unless
  `is hidden`/explicit `*%`), auto-`@_` insertion driven by `auto_signature_uses(&body)`
  (`registration_class_body_method.rs:83-118`), and `::?CLASS` type-constraint substitution
  (`registration_class_body_method.rs:76-82`). All are pure functions of (AST, `is hidden`,
  resolved class name). `is hidden` is a literal trait name on the declaration, statically
  visible in the plan. The `::?CLASS` substitution rewrites `param_defs` *type-constraint
  strings* only — bind-time data, not body-compile input (verification item V1 confirms type
  constraint strings don't alter emitted bytecode).
- `lexically_in_method = true` and the four synthetic params (`accessors_resolve.rs:41-48`).
- Post-passes `compute_may_capture_outer_vars()` + `compute_needs_env_sync()`
  (`accessors_resolve.rs:51-52`).
- Nested subs land in the throwaway compiler's `compiled_functions`, carried as
  `MethodDef::compiled_fns` (`accessors_resolve.rs:57-59`).

**What the body compile does NOT depend on** (so one compile is sound):

- The composing class: role param `T` is an ordinary env variable injected at method entry from
  a class-keyed registry map (`vm_method_dispatch.rs:336-344`,
  `registration_class_compose.rs:178-182`); type-constraint strings resolve `T` through the
  env-alias fallback in `type_matches_value` (`types/type_matching.rs:846-852`). Composition
  substitutes `param_defs` type strings (`registration_class.rs:275-300`) but clones `body` and
  `compiled_code` untouched (`registration_class.rs:313,321`). Bodies are never AST-rewritten
  per composition.
- `::?CLASS`/`::?ROLE`: bound dynamically at method entry (`vm_method_dispatch.rs:279-291`),
  reserved at compile time only as param *names*.
- Param defaults and `where` clauses: evaluated at bind time from `param_defs` AST
  (`types/binding_signature.rs:1143-1145`, `:60-61`), not compiled into the body.

**Sites that must keep the runtime fallback** (legitimately dynamic, no plan or no static name):

- `augment class` (no declaration plan at all — `Stmt::AugmentClass` still indexes `stmt_pool`;
  its walker passes `method_decls: &[]` — `registration_class_augment.rs:1028`).
- `.^add_method`/`.^add_multi_method` (hardcode `compiled_code: None`,
  `methods_classhow_dispatch.rs:807`; served by `populate_uncompiled_method`,
  `vm_call_method_compiled_cache.rs:222-248`).
- Computed declaration names (`class ::($name) { ... }`) — no static package for the key.
- Runtime mixins (`$x but R`) read the role's already-compiled `RoleDef.methods` directly
  (`methods_mixin_dispatch.rs:143`), and role puns copy compiled defs — both become no-ops for
  the bulk pass rather than fallback consumers.

## Design decisions

**1. Per-declaration key on `CompiledMethodDecl`, not a parallel keyed pool.**
`CompiledMethodDecl` (D3-2/D3-7, `opcode.rs:392-412`) gains
`compiled_routine_key: Option<Symbol>`. D3-7 already delivers these structs positionally to both
walkers through the `method_name_chunk_idx` cursor, so the key needs no separate correlation
mechanism and no signature-keyed pool slots — the "multi methods have no signature-keyed pool
slot" complication from the scoping pass dissolves: each multi candidate is its own
`CompiledMethodDecl` carrying its own key, and the registry dispatch table stays
`Vec<MethodDef>` with per-candidate `compiled_code` exactly as today. `None` = "no main-pass
bytecode, use the runtime fallback" (computed name, dynamic shape, or future guard bail).

**2. The main-pass compile replicates the throwaway compiler bit-for-bit; parity first, lexical
inheritance later.** The new compile path (a `compile_method_body` sibling of
`compile_sub_body`) creates a bare sub-Compiler seeded exactly like
`compile_method_def_in_place_with_dist`: `set_current_package(qualified declaration name)`,
`current_distribution` from the enclosing compiler, `lexically_in_method = true`, synthetic
params + compile-time effective params, `compile_routine_closure_body`, then
`compute_may_capture_outer_vars`/`compute_needs_env_sync`. Deliberately NO
`inherit_enclosing_scopes`/`inherit_fold_ctx`/`inherit_outer_code_var_names` in the first
cutover: seeding them would change name resolution inside bodies (outer names would compile to
slot references that don't exist in the method's runtime frame) — that is the future
lexical-capture project, not this box. Parity makes the cutover mechanically verifiable: same
inputs, same bytecode as the runtime compile produces today.

**3. Effective params are computed at compile time by the same shared functions registration
uses.** `effective_method_param_defs`, `has_explicit_named_slurpy`,
`implicit_method_named_slurpy_param` (`registration.rs:23-69`) and `auto_signature_uses` move to
(or are re-exported from) a location both the compiler and registration can call — one
implementation, drift-proof by construction, the D2b/`CompiledAttrDecl` pattern. The compiler
computes them from the plan's static facts (`is hidden` from the literal trait list); the
`::?CLASS` substitution is registration-only (it needs the resolved class name) and by V1 does
not affect the compiled body.

**4. Registration installs by key with an equality guard, falling back on mismatch.**
`class_body_method_decl`/`role_body_method_decl`, after building the `MethodDef` exactly as
today, do: if `decl.compiled_routine_key` resolves in the ambient `CompiledFns` **and**
`cf.params == [synthetic prefix] + def.params`, install
`def.compiled_code = Some(Arc::new(cf.code.clone()))` and `def.compiled_fns = cf.compiled_fns`;
otherwise leave `None` and let the existing bulk pass compile at runtime (unchanged fallback).
The params-equality guard catches every case where registration-time effective params diverge
from the compiler's assumption (however it might arise) and degrades to today's behavior instead
of running mismatched bytecode — the same "degrade, never shift" philosophy as
`vm_register_sub_ops.rs:249-254`.

**5. Key shape and collision handling follow C2.** Keys are interned as
`"{package}::{name}!m/{arity}#{fingerprint:x}"` (a `!m` marker keeps them disjoint from sub
keys; fingerprint = `function_body_fingerprint` over the effective params/param_defs/body, which
also disambiguates same-named multi candidates). Insertion into `Compiler::compiled_functions`
reuses the existing collision rename; `remap_sub_decl_compiled_routine_keys`
(`opcode.rs:3249-3270`) is extended to also rewrite
`class_decl_plans[*].method_decls[*].compiled_routine_key` and the role-plan equivalent, so
nested-compunit import keeps identity exactly as C2 does for subs.

**6. The role N-compiles-to-1 dedup falls out for free.** Under this design the install happens
inside `role_body_method_decl` — i.e. during `register_role_decl`, *before* the
`role_candidates` snapshot is cloned (`registration_role_decl.rs:286-297`). The snapshot then
carries `compiled_code`, composition's clones preserve it (`registration_class.rs:321`), and the
per-composing-class recompile disappears without touching composition code.

**7. `record_type_body_captures` stays untouched initially.** Its analysis compile inherits the
enclosing scopes and package, so its free-var classification differs from the bare parity
compile — reusing one for the other would silently change `type_body_written_lexicals`.
Accepting two main-pass compiles per method body (one analysis, one kept) is the price of a
verifiable cutover; merging them is a follow-up optimization once the parity compile is proven.

**8. Measurable completion gate (ADR-0001 style).** Add a `MUTSU_VM_STATS` counter
`method_body_runtime_compiles`, incremented in `compile_method_def_in_place_with_dist` when it
actually compiles. The box's exit criterion: the counter is **zero** across a representative
`t/` + roast S12/S14 sweep except for the enumerated dynamic shapes (`augment`, `.^add_method`,
computed names), and the `class_dispatch.rs` per-call recompile path no longer fires for
plan-declared methods.

## Slice plan

- **D3-8a — compiler side, additive, nothing reads it.** Shared effective-param functions;
  `compile_method_body` (bare parity compile); per-decl key recorded on `CompiledMethodDecl`
  during `add_class_decl_plan`/`add_role_decl_plan` for statically-named declarations;
  remap-walk extension; the stats counter. No behavior change — validate with full `t/` +
  targeted roast, plus a debug assertion (or test) that the parity compile of a sample corpus
  byte-matches the runtime compile's output.
- **D3-8b — class walker cutover.** `class_body_method_decl` installs by key with the guard of
  decision 4. The bulk `compile_class_methods` pass stays (it now mostly no-ops via the
  `compiled_code.is_some()` short-circuit and still serves fallback shapes). Verify with the
  counter on `t/` and roast S12.
- **D3-8c — role walker cutover.** Same for `role_body_method_decl`; verify the composition
  dedup (counter drops for a `for ^N { class C does R {...} }`-shaped stress) and roast S14 +
  the bundled-battery gate (`scripts/battery-testsuite.sh`), since parametric-role method
  dispatch is load-bearing for Cro/the batteries.
- **D3-8d — fallback narrowing survey. Landed 2026-08-09.** Instrument-and-sweep (C6d precedent):
  a `MUTSU_VM_STATS=1` sweep over all 2974 `t/` files and all 121 whitelisted `roast/S12-*`/
  `S14-*` files found the survey's premise wrong — most remaining hits were NOT the enumerated
  dynamic shapes (`augment`, `.^add_method`, computed names), but a genuine gap: **any**
  class/role declared inside **any** closure body (a `sub`, a bare `{ ... }` block, `if`/`for`,
  an anonymous block passed to `subtest`, ...) unconditionally bailed out of main-pass
  compilation. Root cause: `qualified_class_decl_name`/`qualified_role_decl_name` used
  `self.current_package` to predict the declaration's runtime-qualified name, but EVERY
  closure/sub body compiles under a synthetic STATE-SCOPE pseudo-package
  (`current_package` containing `::&`, assigned unconditionally by `compile_sub_body`/
  `compile_closure_body` purely for `state`-variable key uniqueness — see
  `helpers_sub_body.rs`), which does not track the real runtime package at all. D3-8b's own
  bail-out (added to fix `roast/S12-introspection/walk.t`'s `$?PACKAGE.^name`) treated this as
  unrecoverable and skipped compilation entirely whenever `in_state_scope`. It is recoverable:
  `self.enclosing_package` (already captured before the state-scope override, for `$?PACKAGE`,
  and already propagated unchanged through arbitrarily deep closure nesting) IS the real runtime
  package — a bare block/closure body never itself changes the interpreter's `current_package()`
  (only an explicit `class`/`package`/`module`/`unit` bracketing does, which always sets
  `current_package` directly to the real name, bypassing the mangled form). Fixed by using
  `enclosing_package` as the base package whenever `current_package` is state-scope-mangled, in
  both name-prediction helpers, and dropping the `in_state_scope` bail-out entirely from
  `add_class_decl_plan`/`add_role_decl_plan` (`compiler/decl_plan.rs`) — the helpers now resolve
  correctly either way. Verified: the D3-8a byte-parity unit tests (11/11, including a
  closure-nesting case), the full `t/` suite (2974 files), all 121 whitelisted `S12`/`S14` roast
  files, and the original `walk.t` regression pin all stayed green; a `MUTSU_VM_STATS=1` sweep
  measured the fix's effect directly — across the 121 whitelisted `S12`/`S14` files, the summed
  `method_body_runtime_compiles` count dropped from 494 to 330 (33%), and 6 files (including
  `walk.t` itself, 29 → 0) dropped to zero entirely. **The remaining ~330 hits are a second,
  distinct, already-documented cost, not a new straggler**: `subtest "..." => { ... }` is
  implemented by re-running the block's AST through a **fresh** `Compiler::compile()` call on
  every invocation (`test_fn_subtest` → `eval_block_value` → `compile_block_value_opts`, an
  EVAL-like re-entrant compile unit, confirmed via `rust-gdb` backtrace) — NOT the dedicated
  `Stmt::Subtest`/`SubtestScope` bytecode path (that parser form exists but the common
  `subtest NAME => { ... }` idiom, called as an ordinary function taking a `Pair`, never reaches
  it). Each such fresh compile re-runs `hoist_type_decl_shells`, and the common test-file idiom
  `plan N; class C { ... }` places a runtime statement (`plan`) before the class, which is exactly
  the shelling trigger — so the shell's method-body compile, which by design 2026-08-08's D3-8a/b
  text already documents as "otherwise-redundant" and deliberately left uncompiled (skipped, not
  a bug), fires on every single `subtest` call. This is real, pre-existing, and out of D3-8's
  scope (subtest's whole-block re-compile-per-call is a distinct architectural fact, unrelated to
  method-body compilation); recorded here rather than re-opened as a new ticket, since it is the
  SAME accepted cost D3-8a/b already named, just more visible now that the closure-nesting gap
  around it is closed.

Follow-ups explicitly out of this box, to be filed/kept as separate findings:

- Merging `record_type_body_captures` with the parity compile (single main-pass compile per
  method body).
- Lexical capture for methods of a class declared inside a sub (needs runtime capture
  semantics — the parity compile deliberately preserves today's dynamic-env behavior).
- Persisting the `class_dispatch.rs:497-507` local-clone compile for the residual fallback
  shapes (per-call recompile for `.^add_multi_method` candidates reached through that path).
- `augment class` plans (D3-1's noted gap: `Stmt::AugmentClass` is outside the plan system).

## Verification items (resolve during D3-8a, before the cutover slices)

- **V1**: confirm `param_defs` type-constraint strings do not influence
  `compile_routine_closure_body`'s emitted bytecode (they should be bind-time-only; if any
  native-type slot specialization reads them, the guard in decision 4 must extend to
  `param_defs` equality and the `::?CLASS`-substitution case falls back).
- **V2**: confirm `cx.is_hidden` is derived only from the literal trait list (no computed
  path), so the compile-time `%_` decision is sound.
- **V3**: confirm `resolve_package_distribution(class)` equals the declaring compunit's
  `current_distribution` for bundled batteries/modules (the two derivations differ today:
  `compile_class_methods` resolves per class, `compile_role_methods` per role —
  `accessors_resolve.rs:116,125`); if they can diverge, thread a plan-level distribution
  instead.
- **V4**: byte-parity spot check of the new compile against the runtime compile across a corpus
  (e.g. every method body in `t/` fixtures) — cheap to script, catches any missed seeding.

## Risk notes

The change is architecture-correct-by-construction on the read side (registration behavior is
unchanged whenever the key is absent or the guard fails), so the risk concentrates in silent
bytecode divergence between the parity compile and the runtime compile — addressed by V1-V4 and
the parity-first rule (decision 2). Per the working agreement on semantics-touching changes, run
a local `make roast` before each cutover PR (D3-8b/c), not just `make test`.
