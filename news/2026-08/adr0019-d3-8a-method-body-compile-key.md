# ADR-0019 D3-8a: main-pass method-body compilation (additive)

Every class/role method body was compiled by a throwaway `Compiler::new()` spun up at
registration time (`Interpreter::compile_method_def_in_place_with_dist`,
`src/runtime/accessors_resolve.rs`) — the last routine-body class still compiled this way,
unlike `sub` bodies, which Phase C moved to main-pass compilation with pool-keyed
`CompiledFunction`s back in ADR-0019 C1-C4. This slice (D3-8a, the first of a four-slice plan
recorded in `todo/deep/adr0019-d3-8-method-body-main-pass-compilation.md`) adds the main-pass
compile path but wires nothing to consume it yet — it changes zero observable behavior.

`Compiler::compile_method_body` (new `src/compiler/helpers_method_body.rs`) replicates the
registration-time compile bit-for-bit: a bare `Compiler::new()`, the declaring package, the
enclosing distribution, and `lexically_in_method` for the implicit `%_`/`@_` lexicals — no
lexical-scope inheritance from the main-pass compiler, matching what the throwaway compiler does
today. It is called from `Compiler::add_class_decl_plan`/`add_role_decl_plan`
(`src/compiler/decl_plan.rs`) for every method/submethod declaration with a statically-known
name, and the resulting key is stashed on a new
`CompiledMethodDecl::compiled_routine_key: Option<Symbol>` field (`src/opcode.rs`). The key
stays `None` for a computed method or class/role name (`method ::($n) {...}`,
`class ::($n) {...}`), and for the `__hoisted` forward-reference shell the compiler's own
`hoist_type_decl_shells` pass builds — only the source-order declaration plan compiles, mirroring
how the `sub` side already splits hoisted vs. source-order compilation and avoiding a wasted
second compile of every class/role method body in the program.

Three helper functions — `effective_method_param_defs`, `auto_signature_uses`, and the implicit
`%_`/`@_` slurpy `ParamDef` builders — moved out of `runtime::registration`/
`runtime::methods_signature` (where they were reachable only from the runtime) into a new shared
`src/method_signature_shared.rs` module, so the compiler and the three registration call sites
(`class_body_method_decl`, `role_body_method_decl`, `augment class`) share one implementation
instead of three independently-drifting copies — the same pattern D2b established for
`CompiledAttrDecl`.

Key shape follows the C2 precedent for subs: `"{package}::{name}!m/{arity}#{fingerprint:x}"`,
fingerprinted over the effective params/param_defs/body — which, unlike the sub side, is enough
on its own to disambiguate same-named multi candidates, since each multi candidate already owns
its own `CompiledMethodDecl` and key slot rather than sharing a signature-keyed pool.
`CompiledCode::remap_sub_decl_compiled_routine_keys` (used when a nested compilation unit's
functions are imported into a parent's table) now also rewrites
`class_decl_plans[*].method_decls[*].compiled_routine_key` and the role-plan equivalent, so
nested-compunit import keeps key identity for methods the same way it already does for subs.

A new `MUTSU_VM_STATS` counter, `method_body_runtime_compiles`, is incremented every time
`compile_method_def_in_place_with_dist` actually compiles — the baseline this box's exit
criterion (driving the counter to zero except for the enumerated dynamic shapes: `augment class`,
`.^add_method`, computed names) will be measured against once D3-8b/c install the main-pass
bytecode.

Verification: the design doc's four items were resolved before landing. V1 (a `param_defs`
type-constraint string, including the registration-only `::?CLASS` substitution, does not affect
emitted bytecode) and V2 (`is_hidden` is a literal `is hidden` class trait with no computed path)
were confirmed by reading the parser and by a targeted byte-parity test. V3
(`resolve_package_distribution` vs. the compiler's `current_distribution` field) was reasoned
sound for the case this box actually keys — a class/role declared in its own compilation unit —
since both derivations trace back to the same per-compunit distribution value; a cross-module
scenario was not filesystem-tested and is flagged as residual risk for the D3-8b/c cutover to
re-check. V4, the byte-parity check, is a `#[cfg(test)]` suite in
`compiler/helpers_method_body.rs` covering a corpus of method shapes (plain method, submethod
with attribute binds, typed param, multi method, `is hidden` class, auto-`@_`-detected
signature-less method, method with a nested `sub`, role method, role method's auto-`@_`
non-insertion, the `::?CLASS` substitution case, `$?DISTRIBUTION`) compiled both the main-pass way
and via an actual `Interpreter::run`, asserting the two `CompiledCode`s are `Debug`-identical
(after normalizing the process-global closure-ordinal and Symbol-intern-id noise both compiles
pick up from unrelated background compilation). All pass, and the full `t/` suite plus targeted
roast files (`S12-methods`, `S14-roles`) confirm no observable behavior changed.

The next slices (D3-8b/c) install this bytecode at registration time — `class_body_method_decl`/
`role_body_method_decl` reading the key with a params-equality guard that degrades to today's
throwaway compile on any mismatch — and D3-8d sweeps to confirm the runtime-compile counter
reaches zero for every non-dynamic shape.
