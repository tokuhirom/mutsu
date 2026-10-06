# An atomic scalar op addresses its binding by frame slot, so a shadowing `my atomicint $y` is its own variable

```raku
my atomicint $y = 1;
{
    my atomicint $y = 20;
    say $y⚛++;   # 20 (mutsu used to print 1)
    say $y;      # 21 (was 2)
}
say $y;          # 1
```

Every integer atomic (`⚛++`, `⚛+=`, `atomic-fetch-add`, ...) and the lenient ones
(`⚛$y`, `$y ⚛= v`, `atomic-fetch`, `atomic-assign`, `cas`) found their target by the
variable's bare name: the env entry, then the first slot of the frame spelled `y`. With
shadow slots a same-named inner declaration has a slot of its own, but the name cannot
tell the two apart, so the outer variable answered for the inner one (#12006). The same
lookup served the plain read of an atomic variable (`GetLocal`).

The compiler already knows which binding a call site reaches (`local_map` points at the
innermost declaration in scope). It now tags the helper's target argument with that slot
(`Compiler::emit_atomic_target`, a `WrapVarRef`), only when the current chunk declares the
name in an active scope. The runtime checks the slot against the running frame's own
locals (`own_local_slot`) and boxes or reads that slot's cell (`atomic_local_slot_cell`);
`GetLocal` passes its own slot the same way (`atomic_fetch_local`). A plain string target
(a captured outer lexical, a helper called by name) keeps the by-name lookup, so the shapes
that worked before are unchanged.

Two details that were load-bearing:

- `normalize_call_args_for_target` strips every `VarRef` for a builtin callee, which erased
  the tag before the helper saw it. The atomic `_var` helpers now keep the tag on their
  first argument and unwrap their operands as before.
- `atomic_scalar_cell` moved from `builtins_atomic_shared.rs` (over 1000 lines) to
  `builtins_atomic_lexical_cells.rs`, where its package-scope and unit-scope fallbacks
  already lived.

Pinned by `t/concurrency/thread-lock/atomicint-shadowed-by-inner-declaration.t`.

Not covered: a shape the cell refuses (an `Instance`, a `Seq`, ...) still falls back to the
name-keyed legacy lane, which has no binding identity (ADR-0062).
