# An `our` variable declared in a block that never runs is never installed

Found 2026-09-07 while fixing
`todo/tickets/unreached-end-phaser-lexicals-read-nil-not-any.md`
(`news/2026-09/unreached-end-phasers-see-unassigned-lexicals.md`). That fix
covers `my`/`state` lexicals; `our` is a package symbol and takes a different
path, so it is left over.

## Repro

```raku
if False { our $o = 4; END { say $o.^name } }
say "mainline";
# raku : mainline / Any
# mutsu: mainline / Nil
```

The same holds outside a phaser, which is the more direct statement of it:

```raku
if False { our $o = 4 }
say OUR::<$o>.^name;   # raku: Any   mutsu: (no such symbol)
```

## Root cause

rakudo installs a package symbol when the compunit is **compiled**, so the
`$o` slot exists (undefined) even though the assignment never ran. mutsu
installs the symbol when execution reaches the declaration, so a declaration in
a dead branch installs nothing and a later read finds no binding — which reads
as `Nil`.

This is the same shape as the `END`-installation change of
`news/2026-09/end-phasers-install-at-compile-time.md`: a declaration whose
*symbol* is compile-time but whose *value* is run-time.

## Why it is a ticket

The fix is a compile-time (or pre-run) walk that installs every `our`
declaration's symbol in its declaring package, bound to the type object, before
the body runs — the `EndWalker` in `src/runtime/end_phasers.rs` is the shape to
copy, but the walk has to track the *package* qualification the compiler
applies (`Compiler::qualify_our_variable_name`, which resolves the
`Pkg::&sub/1` state-scope pseudo-packages), and it must not disturb the
existing declaration path when the branch DOES run (an assignment must still
overwrite the pre-installed type object, and a `state`-like re-entry must not
reset it).

## Acceptance

Both repros answer as rakudo does; `t/end-phaser-compile-time-install.t` grows
the `our` row; `our` declarations inside `class`/`role`/`module` bodies, inside
subs (`qualify_our_variable_name`'s pseudo-package case), and `our` with an
initialiser that DOES run all keep their current behaviour.
