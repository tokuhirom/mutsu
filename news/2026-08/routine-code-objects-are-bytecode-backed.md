# A code object made from a registry routine now carries the routine's bytecode

ADR-0019 C6c. About a dozen sites build a code object out of a declared routine —
`&foo`, a `.candidates` entry, `.cando`, the `nextcallee` candidate, the operator
fallback, the `block_stack` entry that `callframe().code` and `&?ROUTINE` read.
Every one of them called `Value::make_sub(def.package, def.name,
def.params.clone(), def.param_defs.clone(), def.body.clone(), …)`, so the
resulting `Sub`'s only executable form was the **AST body**: dispatch either
walked it or re-compiled it on the fly at every call. That was the largest
remaining group of `FunctionDef.body` readers and the reason
`CompiledSubDeclPlan::legacy_body` could not be dropped.

`SubData` now has a `compiled_routine: Option<Arc<CompiledFunction>>` field naming
the routine the code object *is*, and `Value::make_sub_for_routine` fills it from
`FunctionDef::compiled` — the bytecode the declaration plan attached in C3. The
`Sub` dispatch paths that used to compile `data.body` on the fly
(`vm_call_on_value` and the native map-block entry `vm_call_map_block`) now prefer
that bytecode. Nothing executes an AST body for these code objects any more.

## The scoping note this corrects

While scoping the slice, the two calling conventions looked incompatible, and this
file was originally filed under `todo/deep/` saying so:

- `FunctionDef.compiled` is invoked by `compile_and_call_function_def` under the
  *routine* convention (`param_local_slots`, `named_call_plan`, `param_name_syms`).
- `SubData.compiled_code` is invoked by `call_compiled_closure` under the *closure*
  convention (upvalues aligned with `cc.upvalue_syms`, plus the captured env).

`CompiledFunction` contains a `CompiledCode`, so the conclusion drawn from the
types was that handing the inner `code` to the closure dispatch would run a
routine body with no upvalue array and with parameters bound by the wrong plan.

Measuring instead of reasoning from the types showed that is not so, for two
reasons that were already true:

- Both compile paths bake the positional-parameter → slot map into the
  `CompiledCode` itself (`Compiler::record_param_local_slots`, called by
  `compile_sub_body` for a declaration and by
  `compile_closure_body_with_routine_flag` for a closure).
  `CompiledFunction::param_local_slots` is *derived* from `code.param_local_slots`
  by `precompute_param_local_slots`, not independent of it — so a
  declaration-compiled routine body binds its parameters the same way under either
  dispatcher.
- An empty or mismatched upvalue array is explicitly safe: an out-of-range
  `GetUpvalue` index falls back to a live by-name env read (documented at the
  `self.upvalues = data.upvalues.clone()` install in
  `call_compiled_closure_with_topic`).

So C6c needed no calling-convention change and no new dispatch entry point — only
a field, a constructor, and two dispatch reads. The captured env that several of
these sites pass stays load bearing and keeps working, which is what lets an
escaped nested named sub still see its declaring frame's lexical.
`t/routine-code-object-dispatch.t` pins that case along with the rest of the call
surface: optional defaults, named arguments, multi dispatch, `.candidates`
callability, `is rw` writeback, `state`, wraps, and explicit `return`. A Rust unit
test pins the structural claim — the code object shares the routine's
`CompiledFunction` by `Arc` identity rather than a re-compile.

Writing that test also surfaced an unrelated pre-existing bug: `.candidates` is
not in declaration order, and sorting by `FunctionDef::decl_order` does not fix it
because a `multi` is registered twice (hoist pass, then source order) and the
second pass can stamp the pair in the opposite order. Recorded in
`todo/tickets/multi-candidates-declaration-order.md`; the test selects the
candidate it wants by signature rather than by position.

## What is left, and why it is C6d

Breaking on the interpreter's `eval_block_value(&data.body)` while running the
call-surface probe shows exactly one path still executing a routine code object's
AST: `call_sub_value`, reached when a `.wrap` chain routes dispatch through the
interpreter carrier (0 hits once the probe's `.wrap` is removed). That is an
interpreter *execution site*, the same shape as the `eval_block_value(&def.body)`
/ `run_block(&def.body)` carriers: they are reached because an on-the-fly
compilation gate *rejected* the routine, so eliminating them means widening OTF
coverage rather than fixing a dispatch read. Recorded under C6d.
