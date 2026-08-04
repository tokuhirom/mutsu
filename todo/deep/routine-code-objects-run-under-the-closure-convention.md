# A code object made from a registry routine cannot carry the routine's compiled body

Blocks ADR-0019 C6c, and therefore the removal of `CompiledSubDeclPlan::legacy_body`.

## The two conventions

mutsu compiles a routine body twice-shaped, not twice-over:

- `FunctionDef.compiled: Option<Arc<CompiledFunction>>` — attached by the declaration
  plan (ADR-0019 C3). Invoked by `compile_and_call_function_def`, which binds
  arguments through the routine convention: `param_local_slots`, `named_call_plan`,
  `param_name_syms`.
- `SubData.compiled_code: Option<Arc<CompiledCode>>` — attached when a closure value
  is created (`MakeLambda`). Invoked by `call_compiled_closure`, which binds through
  the closure convention: upvalues aligned with `cc.upvalue_syms`, plus the captured
  env.

`CompiledFunction` *contains* a `CompiledCode`, so the types look adaptable. They are
not interchangeable: handing `def.compiled.code` to `SubData.compiled_code` would run
a routine body under the closure calling convention, with no upvalue array and with
parameters bound by the wrong plan.

## Why it matters

About a dozen sites build a code object out of a registry routine —
`&foo`, `.candidates`, `nextsame`'s next candidate, operator fallback, `.wrap`
targets, `Method` objects. Every one of them calls `Value::make_sub(def.package,
def.name, def.params.clone(), def.param_defs.clone(), def.body.clone(), …)` and so
produces a Sub whose only executable form is the **AST body**. That is the single
largest remaining group of `FunctionDef.body` readers (`FunctionDef.body` is down to
47 readers after ADR-0019 C6a/C6b, and this group is most of what is left), and it is
why `legacy_body` cannot yet be dropped from the sub declaration plan.

## What the fix probably looks like

Let a Sub value carry *routine identity* rather than a bare `CompiledCode`, and have
the call path dispatch it through `compile_and_call_function_def` instead of
`call_compiled_closure`. Sketch:

- add a `SubData` variant/field naming the routine (`Arc<CompiledFunction>`, or the
  registry key plus a generation) alongside the existing `compiled_code`;
- teach the Sub call path to pick the routine convention when that field is set;
- keep the closure path byte-identical for genuine closures.

The subtlety is that these code objects are *also* closures in one respect: several
sites pass `self.env.clone()` as the Sub's env, and callers rely on that env being
visible inside the body. A routine-convention call does not consult a captured env
the same way, so the migration has to establish, per site, whether the env is load
bearing or incidental.

## Not to be confused with C6d

The `eval_block_value(&def.body)` / `run_block(&def.body)` carriers are a different
problem: those are reached because an OTF gate *rejected* the routine, so eliminating
them means widening OTF coverage, not fixing a calling convention.
