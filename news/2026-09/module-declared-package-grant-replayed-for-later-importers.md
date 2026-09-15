# A module's declared package is now granted to every importer, not just the first

`Acme::Cow` 0.2 shipped its whole suite as one file, and mutsu failed it on the
second assertion of four:

```
ok 1 - Acme::Cow module can be use-d ok
# You planned 4 tests, but ran 1
Could not find symbol 'Cow::cow'
```

The distribution's `lib/Acme/Cow.rakumod` is `use`d as `Acme::Cow`, but the
package it declares is `Cow`:

```raku
unit module Cow;
class basic { ... }
class cow is basic { ... }
```

So the classes are `Cow::cow`, `Cow::basic`, and nothing about the module's
*name* says so. That distinction is what broke.

## Root cause

The #7797 visibility gate only lets a compunit write `Pkg::thing` when that
compunit `use`d `Pkg` itself, so reaching a package transitively through
somebody else's `use` does not leak it. The grant backing the gate is computed
while the module loads (`granted_packages` in `src/runtime/run_modules.rs`), and
it is computed correctly: it covers the module's own name, the `unit
module`/`unit class` package the file declares, and every type registered under
that prefix.

A re-`use` of an already-loaded module never re-runs that load. It takes the
short-circuit in `use_module_with_tags_inner`, which had its own miniature
version of the grant — `module` and its first `::`-segment, and nothing else.
It could not do better, because the packages a module declares are not
derivable from the name it is `use`d by; only the load knows them.

While the declared package matches the file name that gap is invisible, which
is why it survived. `Acme::Cow` is the case where it is fatal, and `Test`'s
`use-ok` is what makes it fire routinely: `use-ok` loads the module from inside
an `EVAL`, so the *first* importer is an EVAL unit and the script's own
`use Acme::Cow;` is the already-loaded no-op. The script therefore never
received a grant for `Cow` at all. Two plain `use` statements were fine; a
`use-ok` followed by a `use` was not.

This is the same first-load-inside-an-EVAL shape as
[#7806](https://github.com/tokuhirom/mutsu/issues/7806), which fixed the
module's own bare package-name binding going missing. The package grant was a
second thing that load carried and the no-op path could not reconstruct.

## Fix

Record the grant per module at first load (`module_granted_packages`) and
replay it for later importers on the already-loaded path. The recorded set is
exactly what the first load installed into `compunit_visible_packages` — the
declared package, the module name, the types under that prefix, and each of
their top-level segments — so a second importer now sees precisely what the
first one did, and nothing more.

`t/modules/qualified-package-visible-after-use-ok-first-load.t` pins it with
the distribution's own shape: a fixture at `Acme/Cowish.rakumod` declaring
`unit module CowNS;`, `use-ok`'d and then `use`d.

## Result

`Acme::Cow` 0.2 goes from `red` (1 of 4 assertions, dying at line 8) to
`green`: 4 of 4, matching rakudo's baseline on the same file.
