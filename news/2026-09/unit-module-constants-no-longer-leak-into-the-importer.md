# A `unit module`'s file-scope `constant`s and enum values no longer leak into the importer

Closes the last piece of #7555 item 1's divergence (b). #7743 scoped a module's
imports, classes and roles to its own compunit and #7764 scoped a
transitively-`use`d module's package name; what was left was a module's own
file-scope `constant`s and enum values, which were still installed under a plain
name in whatever scope triggered the load.

```raku
# lib/InnerConst.rakumod:  unit module InnerConst; constant INNER-CONST = 42; enum InnerEnum <ALPHA BETA>;
# lib/OuterConst.rakumod:  use InnerConst; unit module OuterConst; constant OUTER-CONST = 7; enum OuterEnum <GAMMA DELTA>;
use OuterConst;             # the importer says only this
say OUTER-CONST;            # rakudo: Undeclared name    mutsu was: 7
say GAMMA;                  # rakudo: Undeclared name    mutsu was: GAMMA
say INNER-CONST;            # rakudo: Undeclared name    mutsu was: 42
say ALPHA;                  # rakudo: Undeclared name    mutsu was: ALPHA
```

The leaked binding was not merely extra, it was arbitrary. Two `unit module`s
declaring the same constant name and both `use`d gave the importer whichever one
loaded first, even though each module's own routines correctly read their own
value.

## Root cause

A module body runs in the **caller's** `env` (`load_module_inner` → `run_block`),
so every file-scope bare name it declares lands under a plain env key in the
loading scope. The end-of-load cleanup in `run_modules.rs` already undid three
classes of those — names `import_module` recorded, a `unit` compunit's own `my`
variables (into `unit_lexicals`), and since #7764 package names the module only
reached through its own `use`. `constant`s and enum values were in none of them:
they were folded into the package-keyed `module_scope_lexicals` but their env
binding was never removed.

`collect_unit_lexical_names` could not cover them — it only walks `Stmt::VarDecl`
and rejects `is_our`, and a `constant` parses to
`VarDecl { is_our: true, custom_traits: [("__constant", _), ...] }`, while an
`enum` is a separate `Stmt::EnumDecl` node it does not look at at all.

## The fix

A new `collect_unit_package_scope_names` walks the compunit's top-level
statements for `VarDecl`s carrying the `__constant` trait and for `EnumDecl`s
(their type name and every variant name), skipping `is export` declarations,
whose lifetime `import_module` already owns. `load_module_inner` removes those
`env` bindings at the end of the load, next to the `leaked_packages` removal.

Unlike the `my` variables, these are deliberately **not** moved into
`unit_lexicals`: they stay in `module_scope_names`, and hence in the
package-keyed `module_scope_lexicals`, which is where the declaring module's own
routines and methods already read them from. Only the `env` binding goes. The
transitive rows fall out for free — the removal runs at the end of *every*
`unit` load, so a nested module's constants are gone before the outer module's
env diff is taken.

## Two measurements that narrowed the issue's own analysis

The issue expected the fix to need a parser-level discriminator between
`constant FOO` and `our constant FOO`, since `--dump-ast` gives an identical
`VarDecl { is_our: true, ... }` for both. Measured against rakudo v2026.07, the
distinction does not arise: inside a `unit module`, `our constant OUR-CONST` is
exactly as invisible to the importer as the bare form — both resolve only as
`M::OUR-CONST` — and the same holds for `our enum`. So both forms are collected
and no new discriminator was needed.

The second measurement confirms the carve-out the issue inferred from the
same-name collision test but had not verified by actually removing the binding:
with the env binding gone, a `unit` compunit's own top-level subs *and* the
methods of classes it declares still read its `constant`s and enum values
through `module_scope_lexicals`. `t/log-async-battery.t` — the regression the
#7555 note worried about — stays green, because `Log::Async` has no `unit`
declarator and this fix is scoped to `unit` compunits. For that no-`unit` shape
rakudo also makes the names visible in the importer, so leaving it alone is the
compatible answer, not a gap.

## Still open

Package-qualified access to a module the importer never `use`d is unchanged:
`InnerConst::INNER-CONST` still resolves in a file that only `use`d
`OuterConst`, where rakudo reports it missing. That is the `GLOBAL::`-versus-
per-compunit *storage* question, which #7764 also left open for package names;
this change scopes visibility of the short name without moving the storage.

`t/module-transitive-use-does-not-leak-types.t` grew from 9 to 27 assertions and
passes verbatim under rakudo v2026.07.
