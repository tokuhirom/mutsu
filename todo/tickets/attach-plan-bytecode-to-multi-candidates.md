# A multi candidate never receives its plan-compiled bytecode

`vm_register_sub_ops.rs` walks a sub declaration plan's `compiled_routine_keys` and
attaches each compiled routine to the registry `FunctionDef` it belongs to — except it
bails out for multis:

```rust
if let Some(def) = self.registry_mut().functions.get_mut(&registry_key) {
    let def = std::sync::Arc::make_mut(def);
    if *multi {
        continue;          // <-- every multi candidate arrives with compiled: None
    }
    ...
    def.compiled = Some(std::sync::Arc::new(adapted));
}
```

The compiler *does* compile every candidate, primary and signature-alternate alike
(`compiler/stmt.rs`, `compile_sub_body_with_deprecation` once per signature), and pushes
a key for each into `compiled_routine_keys`. The missing piece is matching a key back to
the specific candidate it was compiled from: multi candidates live in the registry under
`/n`-suffixed keys and one name owns several `FunctionDef`s, so the non-multi path's
`rsplit_once('/')` key derivation does not identify a candidate.

## Why it matters

Two things wait on this.

**It is the last blocker on ADR-0019 C6d-1.** With `compiled: None`, a multi candidate
reaching dispatch has to be compiled on the fly, keyed by body fingerprint. For a
`state`-bearing candidate of a name declared with signature *alternates* that is wrong:

```
multi sub postfix:<CNT> (AltA $x) | (AltB $x) { state $counter = 0; ++$counter }
```

Those alternates are ONE routine with ONE `state` cell. The compiler shares the cell by
threading one `state_group` into every alternate's compiled body — exactly the bytecode
that never gets attached — so the on-the-fly compile, which knows nothing of the group and
keys on the per-alternate signature, hands each alternate its own cell
(`t/multi-signature-alternates.t` fails). `multi_candidate_state_forces_interpreter`
exists solely to route that shape back to the interpreter entry `call_function_def`, and
that gate is the only reason `call_function_def` still exists. Attaching the bytecode
retires the gate, the entry, and one of the six `&def.body` execution sites.

**It removes a per-call compile from every multi.** A multi candidate invoked through
`call_routine_def` / `compile_and_call_function_def` pays an `otf_compile_function_def`
lookup instead of using bytecode it already has, and the first call of each candidate pays
the compile itself.

## Shape of the work

The plan already knows the order: `compiled_routine_keys[0]` is the primary signature and
the rest follow `signature_alternates` in declaration order. Registration walks the same
list. So the fix is to carry the candidate identity alongside the key — either by having
`register_sub_decl` return the registry key it installed the candidate under, or by
recording, at compile time, the same signature discriminator the registry uses. Then
`adapted.params` / `param_defs` / `return_type` are copied from *that* candidate rather
than from a name lookup.

Care needed:

- Signature alternates share one body but differ in parameters, so the adapted
  `CompiledFunction` must take each candidate's own `param_defs` (and re-run
  `precompute_param_local_slots`), while keeping the shared `state_group` scope baked into
  the code.
- `t/multi-candidate-state-otf.t` pins the opposite requirement for ordinary multis:
  `multi f(Int) { state $c }` and `multi f(Str) { state $c }` are two routines and must
  *not* share a cell.
- `opcode.rs:remap_sub_decl_compiled_routine_keys` remaps these keys on compunit import,
  so whatever identity is added has to be remapped with them.

## Verification

`t/multi-signature-alternates.t` and `t/multi-candidate-state-otf.t` are the two poles.
Once both pass with the gate deleted, remove
`multi_candidate_state_forces_interpreter`, its `multi_alternate_signature_names` set, and
`Interpreter::call_function_def`, and check ADR-0019's C6d-1 box.
