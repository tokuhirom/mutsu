# A typed declaration shadowing an outer binding no longer leaks its constraint out of the branch/loop

A `my TYPE $x` / `my TYPE @a` / `my TYPE %h` declared inside a branch or loop
body that **shadows an already-existing outer binding of the same name** used to
leave its type constraint behind on the outer variable when the body exited:

```raku
sub via-if {
    my $x;                       # outer, untyped, declared FIRST
    if True { my Str $x = "a"; } # inner SHADOWS the outer $x
    $x = 42;                     # raku: fine. mutsu: "expected Str but got Int (42)"
}
```

The container twin behaved the same way (`my @a` outer, `my Int @a` inner, then
`@a.push("x")` after the branch). Measured against real `raku`, which accepts
every one of these, the leak covered **both sigils and every branch/loop
construct** — `if`, `unless`, `else`, `while`, C-style `loop`, `repeat` and
`for` — at mainline as well as inside a routine. Only the two shapes with a
genuine env-restoring scope opcode (a routine body and a plain `{ ... }` block)
were already correct.

This was tracked as `todo/deep/scoped-type-declaration-tags-the-shadowed-outer-value.md`,
opened out of ADR-0042 slice 1, and pinned as ten expected-failing `# TODO`
assertions in `t/typed-constraint-shadow-leak-unfixed.t`.

## Root cause — the name layer, not the value layer

The original ticket blamed the **value**: it concluded that `exec_set_var_type`
tags whatever container currently sits in `env` under the declared name, and
that `tag_container_metadata`'s copy-on-write mutates the *outer* container's
embedded `ArrayData`/`HashData` metadata in place. That diagnosis was wrong, and
the alias probe disproves it directly: after the leak, pushing a bad element
through a differently-named bound alias (`my @z := @a; @z.push("x")`) succeeded
while pushing through `@a` itself died. Enforcement that a different name escapes
cannot be coming from the container's own metadata — it was the name-keyed
`__mutsu_type::<name>` env entry all along.

What actually happened is that nothing ever put the outer binding's metadata
back:

- `exec_block_local_scope_op`'s exit cleanup (ADR-0042 slice 1 step 4) strips
  `__mutsu_type::<sym>` only for names that did **not** exist in `env` before the
  branch. Every shadowing declaration is skipped by design — restoring a shadowed
  outer binding is `pop_loop_local_scope`'s job — and `pop_loop_local_scope` only
  ever restored the *value*, never the metadata.
- Loop bodies (`while`/`until`/C-style `loop`/`repeat`/`for`) compile through
  `compile_body_with_implicit_try` and emit no branch-exit cleanup at all, so
  they leaked for fresh declarations too.

ADR-0042 §10 records a prototype that tried to fix this by extending
`loop_local_saved_env` and measured it to have **no effect**, which is what led
to the value-layer conclusion. The reason it did nothing is ordering: the
compiler emits the type-constraint op **before** the declaration's own
`SetLocalDecl` store, so by the time the store recorded a shadowed binding the
metadata had already been overwritten and there was nothing left to save.

## The fix

`Interpreter::save_type_meta_for_scope_exit` records this name's pre-declaration
`__mutsu_type::` / `__mutsu_hash_key_type::` entries into the innermost
branch/loop scope at the moment `exec_set_var_type` is about to overwrite them —
the one point where the old value is still readable. `pop_loop_local_scope`, which
every branch and every loop form already brackets its body with, then restores
them (or removes them, when the name had none before). First write wins, so a
loop body that re-declares on every iteration still restores the value from before
the loop.

That covers everything declared inside a routine or a block. The mainline shapes
needed one more step, because `Compiler::emit_set_var_type` only picks the
env-only `SetVarTypeScoped` opcode when it knows the VM will restore env at scope
exit — otherwise it also writes the process-global, never-scoped
`var_type_constraints` map. That guarantee now holds for branch and loop bodies
too, so `lexically_in_block` is set while compiling them
(`compile_block_local_branch`, `compile_scope_restored_loop_body`, and the
value-collecting `for`-expression body). Inside a routine this changes nothing —
those bodies were already scoped through `is_routine` — it is purely what extends
the same treatment to mainline. `our`, `&`, dynamics, `__ANON_STATE__` and
package-qualified names keep the both-store opcode exactly as before.

## Verification

`t/typed-constraint-shadow-scope.t` replaces the expected-failing pin with 35
`raku`-verified assertions: the shadow shape for scalars, arrays, hashes and
object hashes across all seven branch/loop constructs, at mainline and inside a
routine; that the shadowed outer keeps its own value/elements; and — the half
that matters for not over-fixing — that the inner declaration still enforces
inside the body (including on a later loop iteration) and that a typed *outer*
container still enforces after an inner shadow has come and gone.

Green with no regressions across `cargo test`, the full `t/` suite (3344 files,
31151 tests), `make roast` (1436 files, 218836 tests) and the bundled-battery
gate (257/272 whitelisted files).

## Residual

Three assertions in the new file stay `# TODO`: a typed outer **scalar** loses
its constraint once any inner declaration of the same name — typed *or* untyped —
has shadowed it in a branch/loop body. That is the mirror-image divergence, it
reproduces identically before and after this fix, and it is scalar-only precisely
because a container carries its constraint on the value while a scalar has
nowhere to put one. It is therefore ADR-0042 slice 2's (cell-carried scalar `of`)
job and is tracked in
`todo/deep/shadowing-declaration-drops-the-outer-typed-scalar-constraint.md`.
