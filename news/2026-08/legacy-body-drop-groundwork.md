# Plan fingerprints survive the body drop; body-less code paths hardened (ADR-0019 C6e-3a)

Groundwork for dropping `CompiledSubDeclPlan::legacy_body`: after this slice,
a plan-derived routine registered with an empty AST body behaves identically
to one with its body attached — validated with a new
`MUTSU_DROP_LEGACY_BODY=1` instrument that simulates the drop at
registration (default off, zero effect). The full `t/` suite (27,519 tests)
passes under the instrument; the drop-mode failures the simulation surfaced
across the roast whitelist (6 files) and the battery testsuite gate
(File::Temp's END cleanup) were each driven to a per-file green, and
`t/legacy-body-drop-instrument.t` pins the representative shapes in CI,
which does not otherwise set the variable.

Identity: the plan now records `body_fingerprint` (structural — plan params +
*effective* param defs + body, exactly what the installed def hashes) and
`RoutineBodyFacts::registration_identity` (line-insensitive, the
redeclaration comparison) at lowering; registration seeds the def's memoized
caches from them, and debug asserts pin seed == lazily-computed value (the
`t/` suite runs on the debug binary in CI, so any divergence fails loudly).
The sites that re-derived `function_body_fingerprint` from def fields —
`callsame`'s candidate filter, proto deferral, `is hidden-from-USAGE` on
both the record and lookup sides — read the memoized fingerprint instead,
and the forward-declaration no-op check reads a plan-recorded
`body_is_empty` fact rather than the AST.

Execution: Sub values built from installed defs now carry the plan's
bytecode (the sub-decl-as-last-statement return value, the `$r` argument a
custom `is` trait_mod receives — the `is Cached` wrap idiom, the
`my method` value declaration, and the block-lexical escape hatch), and
every body-classifying fast path routes a body-less routine Sub
(`data.body.is_empty() && data.compiled_routine.is_some()`) to the real
call path: the map/grep/first batchers, sequence generators and Code
endpoints (a zero-parameter generator like `&subrand ... *` is called with
no args), `Lock.protect` (runs the routine's bytecode in the *current* env,
preserving the fast path's live-state semantics — File::Temp's END cleanup
depends on it), the test-assertion callables (`dies-ok &f`), and `.yada`
(answers from the def's `is_stub` fact).

The drop simulation also mapped the def classes whose bodies are still
load-bearing — the C6e-3b cut-line: plans without resolvable bytecode for
every declared signature, scalar `is rw`/`is raw` params (the
interpreter-carrier rw relay), lvalue routines (`is rw`/`is raw` at the
routine level, or a tail `return-rw` — the assignment machinery extracts
its target from the AST), and NativeCall marshalling traits. C6e-3b makes
the safe-class empty body the default at plan lowering; the ledger is
`todo/deep/c6e-legacy-body-drop-blocked-by-gate-rejected-shapes.md`.
