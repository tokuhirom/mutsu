# One helper for a scope's statements through `SyntheticBlock`

The parser lowers many single source statements into a scopeless
`Stmt::SyntheticBlock` group — a grouped `my ($a, $b)`, `has ($a, $b)`, a
declaration with a `will` trait or a statement modifier, `sub f {...}(...)` —
and those groups nest: `my ($a, $b) := (1, 2)` puts its `MarkBind` group inside
the outer destructuring group. About twenty places asked "what does this scope
declare at its own level?" by hand, fourteen of them with the same one-level
`flat_map` that allocated a `Vec` per statement, the rest with small private
recursions that each picked their own wrappers to look through.

They now share `crate::ast::scope_members` (and `scope_members_mut`): an
iterator over a statement list that looks through `SyntheticBlock` at any depth
without allocating per statement, with two explicit options — look through a
`unit module` wrapper (only the compunit-level scans want that) and keep a
group whole when a caller must compile it as one statement. The class and role
body plans, the method/attribute/`does` precomputes, `augment class`, the
nested-`has` collection, the package-body lexical names, the BEGIN-lift unit
names, the prelude tagging and clash check, the nested-block routine scan, the
phaser `VarDecl` probe and the `sub EXPORT` scans all use it; the walker ratchet
falls from 120 to 109.

Behaviour that changed, each checked against rakudo and pinned in a test:

- A role body now keeps a bind group whole, as a class body already did. Its
  deferred statements run one at a time, so a split `my \x := $y` lost its bind
  context and a later `x = x + 1` died with "Cannot modify an immutable value"
  (`t/oo/role/role-body-bind-group.t`).
- The importer-side approximation of a `sub EXPORT` hook no longer collects a
  sigilless term or an operator declared in an *earlier* bare block of the hook
  — that block's scope ends before the returned `Map` is built, so
  `{ my \twice = 1 }` made `twice 5` a "two terms in a row" error although the
  module exports the routine `&twice`. A bare block that *ends* the hook still
  counts: its value is the hook's return value
  (`t/modules/import-export/export-hook-body-scope.t`).
- The NativeCall prelude clash check sees a `unit module`'s own subs and a
  `proto sub` (unit test next to it).

The `compiler/mod.rs` body-plan tests re-derived the plan length with a private
copy of the flatten that had drifted from the real one (it did not look into
methods or control-flow blocks for nested `has`); they now pin the nested `has`
order and the bind-group shape directly.
