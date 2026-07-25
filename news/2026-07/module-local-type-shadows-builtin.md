# A module-local type named like a built-in now shadows the built-in

Inside a module, a user-declared type whose short name collides with a built-in
type name (e.g. `grammar Grammar`, colliding with the core `Grammar` type)
resolved — when referenced by its **unqualified** bareword name from a sub in the
same module — to the **built-in** type object instead of the module-local
declaration:

```raku
unit module GMod;
grammar Grammar { token TOP { \d+ } }
our sub do-parse(Str $input) is export { Grammar.parse($input) }
```

```raku
use GMod;
say do-parse("123");
# was:  X::Method::NotFound: Unknown method value dispatch (fallback disabled): parse
# now:  ｢123｣
```

`Grammar` resolved to the core `Grammar` type object (`.^name` → `Grammar`,
`.HOW` → `ClassHOW`), so `.parse` was routed to the ClassHOW meta-method
dispatcher, fell through, and raised `X::Method::NotFound`. raku resolves the
unqualified `Grammar` to the module-local `GMod::Grammar` (whose inherited
`.parse` runs). The same shape affects any `class Int`/`class Str`/… declared
inside a module. In the **mainline** the bug did not appear — a top-level
`grammar Grammar {}` registers under the bare name, so it was already found.

## Root cause

In `exec_get_bare_word_op` (`src/vm/vm_var_get_ops.rs`), the resolution chain
checked `is_builtin_type(name)` **before** `resolve_type_in_current_package(name)`.
A user type declared inside a module registers under its fully-qualified name
(`GMod::Grammar`), so `has_type("Grammar")` is false there; `is_builtin_type`
then intercepted and returned the core type before the module-qualification walk
could find `GMod::Grammar`. (In the mainline the type registers under the bare
name, so `has_type` matched first — which is why only the module case broke.)

## The fix

`resolve_type_in_current_package(name)` is now consulted **before** the
built-in-type fallback: a module-local declaration of a name shadows a built-in
of the same name, matching raku. The order is now
`has_type` → `resolve_type_in_current_package` → built-in → …. This is safe for
the common path: `resolve_type_in_current_package` returns `None` immediately at
the mainline (GLOBAL) scope, and returns a qualified name only when the current
package actually declares one — so a bareword `Int` with no shadowing
declaration still resolves to the core `Int`.

Verified against `raku`. Pin: `t/module-local-type-shadows-builtin.t` (covers a
module-local `grammar Grammar` and `class Int`, plus that the core `Int` is
unaffected where nothing shadows it).

## Why it mattered

This was blocker #2 for the `YAMLish` YAML battery candidate
(`docs/batteries/yaml.md`): `YAMLish` is `unit module YAMLish` and declares
`grammar Grammar`, which `load-yaml` calls unqualified as `Grammar.parse($input)`.
With this fix, `Grammar.parse` dispatches correctly (the next blocker is the
grammar itself not matching valid YAML —
`todo/tickets/yamlish-grammar-parse-no-match.md`).

A known remaining nuance: the module-local grammar's `.HOW` still reports
`ClassHOW` rather than `GrammarHOW`, because `package_kinds` is not consulted
under the qualified name here. It does not affect `.parse` (which keys off the
resolved package name) and is tracked separately.
