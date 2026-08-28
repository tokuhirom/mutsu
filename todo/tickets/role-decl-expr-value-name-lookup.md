# `(role R { ... })` as an expression can also yield the wrong same-named role

`Stmt::ClassDecl` used to have this bug (fixed 2026-08 — see
`news/2026-08/class-decl-expr-is-not-a-name-lookup.md`): the compiler's
expression-position arm registered the declaration and then did a NAME
LOOKUP of the bareword (`GetBareWord`) to get the "return value" of the
expression, instead of pushing the type object the declaration just
created. A bareword lookup can resolve to a completely unrelated, same-named
type from a different scope — e.g. a nested `module`/`package` (or `EVAL`'d
code running in a different compilation unit) declares the SAME bare name
that an outer scope already has, and `register_class_decl`/-role/-package
qualify the new declaration by `current_package`, so it is a genuinely
distinct registry entry that the bareword lookup never finds (a direct
bare-name hit on the OUTER entry wins first in
`resolve_bareword_type_name`).

`Stmt::RoleDecl`'s expression-position arm
(`src/compiler/expr_block.rs`, the `Stmt::RoleDecl { name, .. }` match arm
inside `compile_expr_do_stmt`) has the exact same shape: it does
`self.compile_stmt(stmt)` (registers the role), then `GetBareWord(name_idx)`
+ `RoleGroupToCandidate`. Verified with a repro mirroring the class-decl
one:

```raku
unit module MyMod2;
sub my_eval($code) is export { EVAL($code); }
```

```raku
use lib "./tmp/lib";
use MyMod2;
role R { };
say my_eval('(role R { method x { 42 } }).^methods.map(*.name)');
```

prints `()` on mutsu (the pre-existing, methodless outer `R`) instead of
`(x)` (the new role's method) — same root cause, same fix shape available.

`Stmt::Package` (the `class`/`module`/`package` bare-package expression
form) likely has the identical bug too, but was not independently verified
in the class-decl slice; check it with the same repro pattern
(`(package Foo { ... })`/`(module Foo { ... })` — whichever syntax mutsu
accepts in expression position, if any) before assuming it applies.

## Suggested fix

Generalize the mechanism the class-decl fix introduced:
`Interpreter::last_registered_class_key` (set at the end of
`exec_register_class_op`, consumed by the new `PushLastRegisteredClass`
opcode emitted immediately after `RegisterClass` in expression position).
Add the analogous field/opcode for role registration (`exec_register_role_op`
would need to record its own `storage_name`-equivalent), and have the
`Stmt::RoleDecl` expr arm push that INSTEAD of the `GetBareWord` lookup —
`RoleGroupToCandidate` still runs afterward exactly as it does today (it
already correctly converts a role GROUP type object into the specific
candidate; the only change needed is starting from the CORRECT group object
rather than a possibly-wrong bareword hit). Do the same for `Stmt::Package`
if the repro above confirms it needs it.

## Why this is a separate ticket

The class-decl slice's roast repro (`roast/S12-class/attributes.t`) only
exercised the `class` shape; fixing `role`/`package` too was out of scope
for that PR (CLAUDE.md's principle of matching fix size to the actual
failing test, while still recording the adjacent finding rather than losing
it). No roast test currently exercises the role/package shape as a hard
regression, so this is priority-worthy but not urgent — pick it up in the
normal `todo/tickets` oldest-first sweep.
