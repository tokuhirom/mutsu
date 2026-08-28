# A class declaration used as an expression no longer does a name lookup

`(class A { ... })` in expression position used to compile to "register the
class, then resolve the bareword `A`" — a plain `GetBareWord` lookup of the
declaration's own source name. That is wrong in general: a class declaration
used as an expression must evaluate to the type object the declaration just
created, full stop, regardless of what else happens to be registered under
the same bare name elsewhere.

The bug surfaced as `roast/S12-class/attributes.t` test 23 failing under
`MUTSU_REAL_TEST=1` (real vendored `Test.rakumod`): `eval-lives-ok
'(class A { has $.x }).new.x.HOW'` died with `No such method 'x' for
invocant of type 'A'`. `Test.rakumod`'s `eval-lives-ok` runs the code through
a plain `EVAL` call inside `sub eval_exception`, a routine defined in a
*different* compilation unit (a different module) than the test file — and
that routine's package (not the caller's) is what `class A` gets registered
under, exactly like `module M { class A { ... } }` registers `M::A`. The
freshly-declared class was therefore a genuinely distinct registry entry
(`Test::A`, in the real case) from any earlier, unrelated `A` the caller's
own script had declared — but the expression's own bareword lookup still
just asked "what does the name `A` resolve to right now", found the
CALLER's pre-existing `A` first (a direct bare-name hit wins over a
package-qualified one in `resolve_bareword_type_name`), and returned that
instead.

Crucially, no `EVAL` is required to hit this: any `class A { ... }` used as
an expression inside a nested `module`/`package` scope that shares a bare
name with an outer class has the same problem, since both shapes go through
the same `current_package`-based qualification in
`exec_register_class_op`. `EVAL`-from-another-compilation-unit is simply the
shape `Test.rakumod` happens to hit.

The fix removes the bareword lookup entirely for a named
(non-`name_expr`) class declaration in expression position. `Interpreter::
exec_register_class_op` now records the exact registry key it just stored
the class under (`last_registered_class_key` — the same `storage_name` used
for every other post-registration reference in that function, already
correctly package-qualified and/or lexically mangled) into a new
`Interpreter` field, and the compiler emits a new `PushLastRegisteredClass`
opcode immediately after `RegisterClass` to push that type object directly.
No opcode can run between the two (they are always emitted back to back for
this one statement), so the field is always fresh, and re-entrant nested
class declarations (a class body that itself declares a class) cannot
clobber it: each `exec_register_class_op` call writes its own
`storage_name` right before it returns, after any nested registrations
inside its own body have already run and settled.

Fixed in `src/opcode.rs` (new `PushLastRegisteredClass` opcode),
`src/vm/vm_typedecl_ops.rs` (`exec_push_last_registered_class_op`,
and the `last_registered_class_key` write at the end of
`exec_register_class_op`), `src/runtime/mod.rs` (the new field), and
`src/compiler/expr_block.rs` (`Stmt::ClassDecl`'s expression-position
compile arm). Pinned by `t/class-decl-expr-value.t`, which is also green
under real `raku` and covers the mainline, in-a-routine, nested-module, and
EVAL-in-a-different-compilation-unit shapes, plus the pre-existing anonymous
`(class { ... })` path.

The analogous `Stmt::RoleDecl` (and likely `Stmt::Package`) expression-
position arms have the same latent bug — verified with a `role`-flavored
repro of the exact same shape — but fixing them is out of scope for this
slice; see `todo/tickets/role-decl-expr-value-name-lookup.md`.
