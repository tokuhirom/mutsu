# `Foo.counter++` (post-increment on a class-level attribute accessor call, from OUTSIDE a method body) is unsupported

Found while verifying the fix for
`class-scoped-my-dot-attribute-doesnt-persist` (now `news/2026-08/`). Plain
assignment to a class-level attribute accessor already works from outside a
method body (`methods_mut_method_lvalue.rs`'s "Handle class-level attribute
assignment" arm, `Foo.counter = 99`), but post-increment/decrement through the
same accessor call does not.

## Repro

```raku
class Foo {
    my $.counter = 0;
}
Foo.counter++;
say Foo.counter;
```

- `raku`: `1`
- `mutsu`: `Cannot resolve caller postfix:<++>(...); the parameter requires mutable arguments`

Note this is specifically the OUTSIDE-a-method-body case. `$.counter++`
*inside* a method body already works correctly (fixed by the PR this ticket
was found alongside) because it goes through the cell-direct
`read_self_attr_cell`/`write_self_attr_cell` machinery
(`src/vm/vm_var_assign_computed_attr.rs`), which now falls back to
`get_class_level_attr`/`set_class_level_attr` when there is no instance (or
the instance's own attribute map does not have the name). `Foo.counter++` at
the top level is a different code path entirely: it evaluates `Foo.counter` as
a general method-call *expression* and then tries to apply postfix `++` to the
result as an lvalue, which requires a generic "read-modify-write through a
method-call lvalue" mechanism mutsu does not have for this case (real Raku's
`.counter` accessor is presumably itself something the `Scalar` container
machinery can increment through, since it is not `rw` per `.^lookup('counter')
.rw` — confirmed `False` in both `raku` and mutsu — yet it still works).

## Where to look

- `src/runtime/methods_mut_method_lvalue.rs` has the existing "Handle
  class-level attribute assignment" arm (plain `=`) around the
  `has_class_level_attr`/`set_class_level_attr` calls — this is the natural
  place a parallel read-modify-write arm would live.
- Whatever handles postfix `++`/`--`/compound-assignment (`OP=`) when the
  target expression is a `CallMethod` (not a plain `Var`) needs to recognize
  "the callee resolves to a class-level attribute accessor" and route through
  `get_class_level_attr`/`set_class_level_attr` directly instead of trying the
  generic "call `infix:<+>`/`postfix:<++>` on a mutable argument" path that
  currently rejects it.

## Why this is a separate ticket

Small, self-contained, and orthogonal to the inside-a-method-body fix (which
is the actual ticket-mandated deliverable). Left for a dedicated slice so it
does not scope-creep the PR that fixed the primary bug.
