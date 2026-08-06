# A compiled routine now carries its own nested-sub table

ADR-0019 C6e-3c (dropping `CompiledSubDeclPlan::legacy_body`) was blocked on
a defect class found while investigating it: a plan-derived sub can have its
declaration-time bytecode fully compiled (`plan_fully_compiled == true`) and
still fail to *dispatch* that bytecode, because the `CompiledFns` table
threaded through the executing call is often unrelated to the routine being
invoked. This happens whenever a routine is called as a detached `Sub`
VALUE — a map/grep block, an operator fallback, a `.wrap` target, `MAIN`, a
multi-deferral candidate, a reduce/hyper step, `sub EXPORT` dispatch — since
none of those calling contexts naturally has the callee's own compile-time
functions table in scope. The concrete symptom: a nested `sub` declared
inside such a routine's body could never resolve its own compiled routine
key at registration time, so it silently fell back to registering with an
executable AST body instead of running compiled — the `is-eqv` multi
dispatching to `_is-eqv`, which declares a nested `sub test-eqv`, was one
real instance.

`CompiledFunction` now carries `compiled_fns: Option<Arc<CompiledFns>>` —
the nested-sub subtree it was compiled alongside, mirroring the
`MethodDef.compiled_fns` fix from #5982 but generalized from methods to
plain subs. It is populated at both places a `CompiledFunction` is built
(the named-sub compile path in `compiler/helpers_sub_body.rs`, and the
on-the-fly compile path in `vm::vm_call_dispatch::otf_compile_function_def`)
by having `Compiler::import_compiled_functions` return the post-remap
import set it used to discard, instead of a caller having to re-derive it.

An audit of the ~17 call sites that previously substituted
`CompiledFns::default()` for a routine they had in hand now prefers
`cf.compiled_fns` first, falling back to an empty table only when the
routine declares no nested sub. Several of them funnel through
`compile_and_call_function_def` and `call_function_compiled_first`, so
fixing those two functions transitively fixed `sub EXPORT` dispatch, the
`prefix:<~>` operator-overload probe, and the numeric/set operator
fallbacks as well. The remaining `CompiledFns::default()` sites are either
the pre-existing, already-correct `MethodDef.compiled_fns` reads, or a raw
closure's `compiled_code` (out of scope: `SubData` carries no equivalent
field, and a closure is never plan-derived).

Validated by re-running the same experiment an earlier (reverted) attempt
at dropping `legacy_body` used: force every plan-derived def to register
body-less regardless of whether its compiled routine key resolves. Before
this fix that surfaced eight real regressions across `t/`
(`t/escaped-our-sub-*.t`, `t/our-sub-block-lexical-capture.t`,
`t/is-eqv.t`, `t/module-sub-otf-*.t`, `t/mustache-battery.t`,
`t/digest-battery.t`, `t/export-sub-infix-operator-closure.t`,
`t/indirect-declarator-names.t`). With this fix in place, only one file
still fails under the same forced experiment — a `proto sub` nested inside
an OTF-compiled module sub, isolated to `RegisterProtoSub`'s separate,
still-`stmt_pool`-indexed registration mechanism (ADR-0019 slice C8,
explicitly out of C6e-3c's scope). The full `t/` suite (27718 tests) and
`make roast` (218774 tests) both pass unmodified with the carrier field
landed. Details and the narrowed remaining blocker:
`todo/deep/c6e-legacy-body-drop-blocked-by-gate-rejected-shapes.md`.
