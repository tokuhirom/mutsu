# `.^lookup` / `.^find_method` answer the `Mu` type object for an absent method, not `Nil`

`Metamodel::MethodContainer.lookup` hands back the **`Mu` type object** when no method
of that name exists anywhere in the MRO. mutsu returned its internal `Nil`, so
`Int.^lookup("does-not-exist")` gisted as `Nil` where Rakudo prints `(Mu)`.

The distinction is not purely cosmetic even though both are undefined: `//`, a boolean
test, and `.defined` behave identically either way, but `.^name`, `.raku`, and an
`=== Mu` identity check do not — and those are exactly what
`Type/Metamodel/MethodContainer.rakudoc` asserts.

## The fix

Both not-found branches in `src/runtime/methods_classhow_dispatch.rs` (`lookup` and its
stricter sibling `find_method`, which is `Mu` in Rakudo too) now return a shared
`mop_absent_method()` helper producing `Value::package(Mu)` instead of `Value::NIL`.
The ticket predicted this was a single-site change and it was — the only widening was
covering `find_method` as well, since it has the same Rakudo sentinel and the same
`unwrap_or(Value::NIL)` shape.

Pinned by `t/metamodel-introspection.t`, which checks `=== Mu`, `.^name`, and
`.defined` on an absent name, and also pins the surrounding contract the ticket did not
state: `.^lookup` finds an *inherited* method (`Looked.^lookup('say')`), finds a multi
*dispatcher* by name, and does **not** see a private method (`Mu` again).
