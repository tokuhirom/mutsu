# The import scope no longer rolls the routine tables back

`pop_import_scope` used to hold the function, proto-function and proto-sub tables as
they were when a block's `use` opened, and at the block's exit it filtered every
registered routine against that snapshot (plus a shadow/restore map for hidden
families, a write counter and an installed-key list to speed the filter up). Since
a block's plain routines, multi families and operators bind in its lexical import
set (ADR-8751), none of that was load-bearing any more: a measurement over all of
`t/` and the whitelisted roast found only two tests still reading it.

Both had a real cause, fixed here instead of kept alive by the rollback:

- An import that binds a plain routine lexically also went on to bind the
  `GLOBAL::name` proto a *different* bare-file module left behind as the same
  name's family, so an exported wrapper lost to the proto it `use`s internally
  (`exported-wrapper-same-name-imported-proto.t`, #8798). A routine bound plainly is
  now the whole import.
- A registry-path import inside a block that declares its own multis of the name
  needed the enclosing family hidden for the block's extent. That hiding is now an
  explicit write log on the scope: each registry write an import makes records the
  value it replaced, and only the importing unit's own scope undoes them at the pop.
  A module that loads inside the block keeps its imports (NativeCall's
  `map_return_type` family).

The pop is now O(writes) instead of O(registered routines), the foreign-`MAIN` drop
stays, and a block-scoped `use` of a bare-file module keeps its candidate-less proto
(a `todo` in `imported-candidate-less-proto.t` is gone). The `env`-key half of the
rollback is still there: `EVAL`, `MY::`/`UNIT::` listings and package names still read
those keys (#14140).
