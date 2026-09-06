# A typed `my` declaration was only in effect at block start in one of nine block forms

Raku's "declarations are in effect at block start" rule means an assignment
running *before* the textual declaration still sees its type constraint:

```raku
{
    try { EVAL '$x = "abc"' };   # X::TypeCheck::Assignment: expected Int, got Str
    my Int $x;
}
```

mutsu implements that with a compiler pre-pass, `hoist_typed_var_decls`, which
emits the declaration's `SetVarType` / `SetVarTypeScoped` op at block entry so
the constraint is registered before any of the block's statements run.

The pre-pass was wired into exactly **two** of the compiler's block-compilation
entry points: the mainline (`compiler/mod.rs`) and the value-position inline
block path (`compile_block_inline`). Every other way a block body reaches the
compiler skipped it — the statement-position bare block (`OpCode::BlockScope`),
a block carrying `use` or `let`, routine and closure bodies, `for` / `while` /
C-style `loop` bodies, and `if` / `else` branches. In all of those the declared
type simply did not exist until execution reached the declaration statement.

## What made it look provider-specific

`todo/tickets/eval-assign-loses-a-later-block-declarations-type.md` recorded the
symptom as a *vendored-`Test`-only* failure of `roast/S04-declarations/my-6e.t`
test 61 ("also a type error"), bisected to `e49b300` (ADR-0042 slice 3, which
retired the global name-keyed type-constraint side table). Both parts of that
framing were surface effects:

* **Provider dependence** came from the *shape* of the source, not from `Test`.
  The block at `my-6e.t:210` is the last statement of its enclosing scope only
  when nothing follows it; under the real `Test` module the file's `is` /
  `dies-ok` calls put statements after it, so the block compiled through the
  statement-position `BlockScope` arm (no hoist) instead of the tail-position
  inline arm (hoist). Reduced, the repro needs no `Test` module at all — the
  trailing `say` is the entire difference:

  ```raku
  my $x = 0;
  {
      my $err = "NO-DIE";
      try { EVAL '$x = "abc"'; CATCH { default { $err = .^name } } };
      say "err=", $err;
      my Int $x;
  }
  say "trailing statement";   # delete this line and mutsu was already correct
  ```

* **The bisect** pointed at slice 3 because the retired global side table used to
  paper over the missing hoist: line 228's `throws-like 'my Int $x = "abc";'`
  EVALs a `my Int $x` declaration whose constraint the global map kept
  process-wide, so line 229's `EVAL '$x = "abc"'` found it by name afterwards.
  Slice 3 correctly deleted that cross-scope leak, which exposed the real gap.
  The hypothesis in the ticket ("the constraint is only reachable through a
  container some earlier statement materialized") was not what happened: the
  `SetVarType` op was never emitted at all. `rust-gdb` breakpoints on the
  `SetVarType` arm and on the `SetGlobal` type-check confirmed it — the
  constraint lookup ran, and there was nothing in the env for it to find.

## The fix

`hoist_typed_var_decls` is now called from every block-compilation entry point:

* `compiler/stmt.rs` — the `Stmt::Block` `BlockScope`, `LetBlock` and
  `PushImportScope` arms.
* `compiler/helpers_control_flow.rs` — `compile_body_with_implicit_try_inner`
  (the shared entry for `if`/`else` branches and statement-form loop bodies) and
  `compile_scope_restored_body_value` (the value-collecting loop-body form).
* `compiler/helpers_sub_body.rs` — routine and closure bodies, right beside the
  existing `hoist_sub_decls` call that implements the same "declarations are in
  effect at block start" rule for named subs.

## The hoist had to become value-neutral first

Wiring the existing `SetVarType` op into those sites was not enough, and the
`t/typed-constraint-shadow-scope.t` / `t/typed-constraint-store-matrix.t` pins
caught why immediately: `exec_set_var_type` does not only *register* a
constraint, it also writes the value bound to the name — seeding a Nil scalar
with the type object, and stamping `value_type` onto the bound
`ArrayData`/`HashData`. At a *declaration* that value is the declaration's own,
but at a *block-entry hoist* the declaration has not run, so whatever is bound
to the name still belongs to an enclosing scope. Hoisting inside a shadowing
block therefore mutated the outer variable:

```raku
sub f { my @a; if True { my Int @a; @a.push(5) }; @a.push("x") }
# "Type check failed for an element of @a; expected Int but got Str" —
# the OUTER @a had been stamped Int at branch entry.
```

The block-exit restore (`save_type_meta_for_scope_exit` /
`pop_loop_local_scope`) puts the *metadata* back but cannot undo a write to the
value itself, so this was unfixable at the exit end.

The hoist now emits its own opcode, `SetVarTypeHoisted { name_idx, tc_idx,
scoped }`. It picks the same registration store `emit_set_var_type` would (so a
scope that has a frame still gets the env-scoped ADR-0042 slice-1/3 registration
and the constraint still dies with its block), and is otherwise value-neutral:
no container tagging, no `clear_atomic_array_state`, and the Nil→type-object
seeding only for a name that has **no** binding at all — which is exactly the
shape the hoist exists for, where the pending declaration is the only binding
(`roast/S04-declarations/my-6e.t` "unreached declaration in effect at block
start" reads `$x ~~ Int` from a `my Int $x` execution never reaches).

## Result

A dies-ok matrix measured against Rakudo went from 1/9 block forms correct to
9/9; `t/typed-decl-hoist-block-forms.t` pins all of them plus `Str`, typed-array
and typed-hash constraints. `roast/S04-declarations/my-6e.t` and
`roast/6.c/S04-declarations/my-6c.t` now pass under **both** the native and the
vendored `Test` provider, which was completion criterion 1 of
`todo/deep/vendor-real-test-module.md`.

## Still open, deliberately out of scope

`hoist_typed_var_decls` skips `state` and `our` declarations, so

```raku
my $x = 0;
{ try { EVAL '$x = "abc"' }; state Int $x; }
```

still does not type-check where Rakudo does. `our Int $x` is a compile-time
error in Rakudo ("Cannot put a type constraint on an 'our'-scoped variable") and
mutsu accepts it; both are separate from the hoist wiring and are recorded in
`todo/tickets/state-and-our-typed-declaration-hoist.md`.
