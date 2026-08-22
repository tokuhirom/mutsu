# A module's `EXPORT::DEFAULT` namespace isn't a real, symbolically-navigable package

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Language/using-modules/code.rakudoc:95`).

## Root cause

Raku models a used module's exported symbols as living in a real nested package,
`ModuleName::EXPORT::DEFAULT`, reachable like any other package — including via symbolic
package lookup (`::("...")`). mutsu's `use`/`require` machinery evidently copies exported
symbols directly into the importing scope (confirmed: an unqualified `::("&ok")` lookup
after `use Test` succeeds), but never materializes the `ModuleName::EXPORT::DEFAULT`
package itself as a queryable stash — so a *fully-qualified* symbolic lookup through that
path fails, and the module's own package stash (`.WHO`) doesn't even list `"EXPORT"` as a
key.

## Minimal repro

```raku
use Test;
my &mmk = ::("Test::EXPORT::DEFAULT::&ok");
say &mmk;
```

- `raku`: resolves to the `Test` module's exported `&ok` sub.
- `mutsu` (`target/debug/mutsu`): `No such symbol 'Test::EXPORT::DEFAULT::&ok'`.

Confirmed narrower pieces that DO work (isolating the gap to the qualified-package path
specifically):

```raku
require ::("Test"); say "loaded";     # OK — dynamic module loading works
use Test; my &mmk = ::("&ok"); say &mmk;   # OK — unqualified symbolic lookup works
```

And confirmed the module's own stash doesn't expose the `EXPORT` sub-package at all:

```raku
use Test; say Test.WHO.keys;
```

prints an unrelated set of keys (no `"EXPORT"` among them) rather than the module's real
export namespace structure.

## Why this is `todo/deep`, not a shallow slice

- This is a module-system / package-stash architecture gap, not a missing individual
  function: mutsu's `use Module` import path apparently does NOT model
  `Module::EXPORT::DEFAULT` (and by extension `EXPORT::MANDATORY`/tag-named export groups)
  as real nested packages with real stashes at all — it just copies symbols into scope
  by name. Making `::("Module::EXPORT::DEFAULT::&name")` work requires either
  constructing that nested package structure for every `use`d module (so `.WHO`/symbolic
  lookup can walk it), or special-casing the `::(...)` resolver to recognize an
  `EXPORT::DEFAULT`-shaped qualified name and redirect it into whatever internal export
  table mutsu actually uses today.
- Any fix here touches the module-loading/import path (`use`/`require` machinery, package
  registration) broadly enough that it needs its own design decision about how deep to
  model the export-tag namespace (`DEFAULT` only, or also user-declared export tags like
  `is export(:MANDATORY)`), not a one-file patch.

## Affected files (starting point)

Module-loading/import registration (wherever `use`/`require` copies a module's exported
symbols into the importing scope) and the `::(...)` symbolic package/symbol resolver.
