# A `class A::B` binds the bare name `B` importer-scoped, not globally

Declaring a class or role whose name is already qualified (e.g. `class
Cro::Hdr { }`, or `class C1` compiler-prequalified to `M::C1` inside `unit
module M`) used to install its short name (`Hdr`, `C1`) into the interpreter's
single flat, global `env`, visible from anywhere in the process for the rest
of the run:

```raku
class Cro::Hdr { }
say Hdr.^name;   # raku: Undeclared name 'Hdr'   mutsu (before): Cro::Hdr
```

and letting it silently clobber an unrelated same-short-name declaration in a
totally different scope (an enum member inside a `supply` block, for
example). This was the live blocker behind `Cro::HTTP::ResponseParser`/
`Cro::HTTP::RequestParser`, where `Cro::HTTP::Header` kept stealing the
bareword `Header` from an unrelated `my enum Expecting <StatusLine Header
Body>`.

## Attempt #1 (reverted): ancestor-chain walking was not enough

A first attempt moved the alias into `package_type_aliases`, keyed by the
*declaring* package, resolved by walking only the ancestor chain of
`current_package`/`method_class_stack`. That fixed the ticket's own examples
but broke a real, working ecosystem module: `DBDish::Pg::Native` declares
`class PGconn` (a NativeCall handle type); sibling class `DBDish::Pg` `use`s
that module and references `PGconn` bare from its own `method connect(...)`.
`DBDish::Pg::Native` is a *sibling* of `DBDish::Pg`, not an ancestor, so no
ancestor-chain walk can ever reach it — CI's bundled-library gate failed 4/4
times on `DBIish/38-pg-errors.rakutest`. The PR was reverted (closed
unmerged) and the root cause recorded so the next attempt started from the
right direction instead of repeating it.

## The real fix: importer-scoped aliasing

The declaring-package write from attempt #1 is kept (it correctly covers
`URI::Path` referenced bare inside `URI`'s own methods — same package,
different declaration site). Additionally, `load_module_inner`
(`src/runtime/run_modules.rs`) now also records each newly-declared
class/role's short name against the **importer** — whichever `unit
module`/`unit class`/`unit package` issued the `use`/`need` that triggered
the load — mirroring real Raku's actual rule: a name a file brings into scope
via `use` is visible in *that file's own lexical scope*, independent of
package ancestry.

The importer is *not* `current_package()`: a file-top-level `use` runs before
its own unit's body registration sets `current_package` to that unit's name
(`run_class_body` only does that once the class starts registering, and
`use` runs ahead of that — a file-scoped `use` is effectively hoisted ahead
of the class's own registration). The correct signal is
`unit_module_loading_stack`, which already holds the currently-loading
compunit's own unit name, pushed before its body runs — so a *nested* `use`
reached from inside that body sees its own unit name still on top.

Finding the importer needed one more fix: `detect_unit_package_name` only
recognized `Stmt::Package { is_unit: true, .. }` (a `unit module`/`unit
package`), not `Stmt::ClassDecl { is_unit: true, .. }` (a `unit class`) — a
different AST variant with its own `is_unit` field. DBIish's driver classes
(`unit class DBDish::Pg ... does DBDish::Driver;`) are exactly this shape, so
`unit_module_loading_stack` stayed empty for any `use` reached from inside
one, silently defeating the importer-scoping for the one case that mattered
most. Fixed by checking both variants.

A second write site covers a **repeat** `use` of an already-loaded module:
`load_module_inner` only runs on first load (nothing new to register on a
second `use`), so `use_module_with_tags_inner`'s already-loaded fast path
(`src/runtime/runtime_module.rs`) now separately copies the aliases already
recorded against the module's own name into the *new* importer's own entry —
this is what makes `DBDish::Pg`'s own `use DBDish::Pg::Native;` work even
though `DBDish::Pg::ErrorHandling` already triggered the first load earlier
in the same run.

Verified against the real DBIish dist (`38-pg-errors.rakutest`, run against a
live Postgres container) end to end, plus the original ticket's own examples,
`URI::Path`, a role variant, and two full local `t/` suite runs. New pin:
`t/package-short-name-alias-scope.t`.

## Related

`todo/tickets/class-nested-my-class-clobbers-outer-short-name.md` was the
*class-body* half of this problem, fixed earlier (see
`news/2026-08/class-body-type-scope.md`): a type declared in a class body has
its short-name binding restored when the body ends, and nested
classes/roles/subsets are recorded as class-scoped short names so the class's
own methods still resolve them (`t/class-body-type-scope.t`). This was the
remaining *package* half.
