# `do { use M; ... }` scopes its import, and an imported `proto` is rolled back with it

An import is lexical to the block that asked for it. mutsu had a mechanism for
that — `push_import_scope` / `pop_import_scope` around a bare block — but two
halves were missing, and `roast/S32-list/skip.t` needs both. That file imports
`Test` selectively, through exactly the shape the two bugs met in:

```raku
# By default, Test exports a "skip" sub, which interferes with the "skip"
# functionality we want to test here.  Hence the selective import here.
BEGIN my (&plan, &subtest, &is, &is-deeply, &throws-like) = do {
    use Test;
    (&plan, &subtest, &is, &is-deeply, &throws-like)
}
```

## 1. A `do { }` block never opened an import scope

`compile_stmt`'s `Stmt::Block` arm emitted `PushImportScope`/`PopImportScope`
when the block contained a `use`, but `compile_do_block_expr` did not — so every
import inside a `do { }` leaked into the enclosing scope. With mutsu's native
`Test` provider in charge the leak was invisible on this file, because the native
`skip` handler disambiguates the list-skip shape (`skip_call_is_list_skip`); with
the real module (`MUTSU_REAL_TEST=1`) the file aborted six assertions in, at

```
skip() was passed a non-integer number of tests.  Did you get the arguments
backwards or use a non-integer number?
  in sub skip at modules/Rakudo-Core/lib/Test.rakumod line 396
```

## 2. `pop_import_scope` did not roll back the proto tables

Closing the first half exposed the second. `pop_import_scope` restored
`functions` and `classes`, but an import also writes the two *proto* tables:
`import_module` inserts the importing package's alias into `proto_functions`
**and** into the `proto_subs` name set, because `has_proto` reads the latter.
Neither was snapshotted, so `Test`'s `proto sub skip(|) is export` stayed visible
as `GLOBAL::skip` after the block exited.

That is not a "wrong routine gets called" bug — `skip` did dispatch to the core
list routine. It is an *argument-shape* bug. `normalize_call_args_for_target`
keeps call arguments in their raw, `VarRef`-wrapped form when a user routine of
that name is declared (so `is rw` can bind through) and unwraps them otherwise.
The stale proto kept the first branch alive, so `builtin_skip` received `@array`
as a `VarRef` instead of an `Array`, fell through its flattening match to the
catch-all arm, and skipped 5 elements of a one-element list:

```
skip(0, @array)   # (a b c d e f g h,)  — one element, the array itself
skip(5, @array)   # ()
```

Both proto tables now use the same keep-rule the function table already used:
entries added since the push are dropped, except a module's own
package-qualified definitions (`Test::skip`), which persist so a later
block-scoped re-`use` in a sibling block still has something to alias. The
`ImportScopeSnapshot` 6-tuple became a named struct in the process — every symbol
table an import writes into has to be listed there, and a tuple made that
list easy to leave incomplete.

`roast/S32-list/skip.t` now passes under both providers (55/55). Pin:
`t/use-in-do-block-is-scoped.t` with fixture `t/lib/ScopedProtoExport.rakumod`,
which exports a `proto sub head(|)` that deliberately collides with the core
`head` listop — the collision is what makes the leak observable without leaning
on `Test` itself.

## What this did *not* fix

`{ use Test; } say &ok.defined` still answers `True`. That one is not the import
scope at all: mutsu's native TAP provider is gated on `loaded_modules`, which is
deliberately never rolled back, so the native `ok` stays reachable whether or not
the module's own routines are in scope. It disappears with the native provider
itself (step 3 of `todo/tickets/vendor-real-test-module.md`).
