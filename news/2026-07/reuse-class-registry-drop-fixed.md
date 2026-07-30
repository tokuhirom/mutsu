# Re-`use` in a sibling block no longer loses a module's classes

When the same module was `use`d in two sibling blocks, method calls on a class
the module declares failed in the second block with
`X::Method::NotFound: ... new on ScanCacheHelper::ScanCacheThing`, while
smartmatch type checks and exported sub calls kept working. Found while
pinning the module export scan cache (`t/module-export-scan-cache.t`), filed
as `todo/tickets/reuse-in-block-class-method-dispatch.md`, and root-caused the
same day.

## Root cause

`pop_import_scope` (the lexical import scope a block pops on exit) restored
`registry.classes` to its pre-block snapshot wholesale. The used module's own
package-qualified classes were dropped with the imported aliases — but
`loaded_modules` is never rolled back, so the module was left half-loaded: a
later block-scoped re-`use` is a no-op that cannot re-register the classes,
and the second block's `.new` fell through class dispatch to the bare-package
fallback error. The `functions` registry one line above had this exact bug
fixed already (its retain deliberately preserves `::`-qualified module
definitions so a sibling-block re-`use` can re-import from them); the classes
retain simply lacked the same exception.

## Fix

`pop_import_scope` now keeps package-qualified (`::`-containing,
non-`GLOBAL::`) class entries, mirroring the functions retain. Bare imported
aliases still go out of scope with the block, so lexical import semantics
(roast S11-modules/lexical.t) are unchanged.

Pinned by `t/module-reuse-class-in-block.t` (raku-verified: 4/4 on rakudo).
`t/module-export-scan-cache.t`'s second block now asserts real instantiation
instead of sidestepping with a `~~ Mu` check.
