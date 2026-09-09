# `need Foo;` no longer suppresses exports `Foo` imports for itself

`need Foo;` loads a compunit without importing its exports into the caller —
implemented by setting `suppress_exports` for the whole duration of the load
so `Foo`'s own `is export` declarations are never registered as importable.
That flag was a single unscoped boolean, though, so it stayed set for every
nested `use` statement `Foo`'s own body executed too. A `Foo.rakumod` that
itself said `use Bar;` therefore had `Bar`'s exports silently dropped: `Bar`'s
`is export` subs never reached `exported_subs`, so `Foo`'s own `use Bar;`
found nothing to import, and any of `Foo`'s methods calling a `Bar` routine
died with `Unknown function`.

`Interpreter::use_module_with_tags` now suspends `suppress_exports` for the
duration of its own nested load, restoring the ambient value afterward. An
explicit `use` always wants ordinary export semantics, regardless of whether
it happens to run inside a `need`-loaded compunit's mainline — only the
directly `need`ed module's own exports should stay unregistered.

This was found via the bundled-library gate running under the vendored `Test`
module (`MUTSU_REAL_TEST=1`), where four whitelisted `DBIish` files (used
through `need DBIish::CommonTesting;`) called `diag` from a method of that
compunit and hit exactly this gap — `DBIish::CommonTesting` itself does
`use Test;`. All four now pass under the vendored provider with no database
server present. Pinned by `t/need-preserves-nested-use-imports.t`, a minimal
`need`-loaded class whose own `use` of a sibling module must resolve from one
of its methods.

Closes #7805.
