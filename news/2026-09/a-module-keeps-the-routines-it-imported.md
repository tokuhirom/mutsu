# A module keeps the routines it imported

Three separate ways a module could lose access to the routines *it* had
imported, all with the same shape: the name was registered while the module
loaded, some later scope teardown swept it away as if it belonged to that
scope, and `loaded_modules` — which is never rolled back — made the module's
next `use` a no-op that could never put it back. The module stayed "loaded"
while its own bodies died with `Unknown function`.

Found by running the bundled-library gate under the vendored upstream `Test`
module (`todo/deep/vendor-real-test-module.md`), which is how a `use-ok`
becomes an `EVAL "use ..."` and a `need` reaches a module that opens with
`use Test;`. None of the three is about `Test`: each reproduces on plain
user modules, and two of them reproduce with no `Test` in the program at all.

## `need` suppressed the exports of everything the needed module used

`need Foo` sets `suppress_exports` so that FOO's own `is export` declarations
do not publish to the needer. The flag stayed on for the whole load, so a
`use Bar` inside Foo's body silenced Bar's exports too — and then there was
nothing for `import_module` to bind into Foo's own scope.

```raku
# Consumer.rakumod
use Provider;              # exports `provided`
unit class Consumer;
method go() { provided(42) }
```

```raku
need Consumer;
Consumer.new.go;           # Unknown function: provided
```

`use Foo` masked it: the needer's own copy of the import made the name
resolvable by accident. `need DBIish::CommonTesting` — whose file opens with
`use Test;` — died with `Unknown function: diag` inside its own method.

`use_module_with_tags` now saves and clears `suppress_exports` around the load,
exactly as it already did for `pending_dist_selectors`, and for the same
reason: a transitive `use` inside a module body is a different question from
the one the outer `need` asked.

## A scope restore reclaimed a module's own imports

mutsu keys both a module's own imports and an importing *block's* bare aliases
as `GLOBAL::name`, so a registry restore could not tell them apart and dropped
both. Dropping the block's aliases is right (`{ use Foo }` must not leave
`foo()` callable, roast `S11-modules/lexical.t`); dropping the module's is not.

`module_owned_global_fns` is that missing distinction. `import_module` records
the alias when a module load is on the stack — never for a program-level block
— and `reinstate_module_functions` puts those back alongside the
package-qualified ones it already restored. The NativeCall prelude's ambient
helpers (`nativecast`, `nativesizeof`, … registered as `GLOBAL::name` under
`PRELUDE_SUB_TRAIT` so a body under any package can call them bare) join the
same set.

`use-ok 'NativeHelpers::Blob'` followed by the real `use` was the live case:
the EVAL's registry restore took `GLOBAL::nativecast` with it, and the module's
own `BODY_OF` died on the next call.

## `use NativeCall` stopped publishing its package symbol on a re-`use`

`register_nativecall_exports` writes `env<NativeCall>` so `::('NativeCall')`
resolves to the package, but did it *after* an early return keyed on
`exported_subs`. `env` is scoped and the export tables are not, so once a scope
restore had dropped the env half, every later `use NativeCall` took the early
return and never rewrote it. The write moved ahead of the early return.

The same class of loss for a module reached transitively: `module_package_globals`
recorded only env keys containing `::`, which skipped a bare package symbol a
module's load had installed. It now also records an unqualified key whose value
is a package/type object — a module symbol, not a lexical — so the re-`use`
path's existing `reinstate_module_package_globals` can restore it.
`NativeLibs`' `::('NativeCall')` probe was the live case.

## Effect

Six of the nine bundled-library rows that regressed under the vendored `Test`
module are fixed: `DBIish`'s four common-testing suites, `NativeHelpers::Blob`
and `NativeLibs`. The bundled-library gate goes from 283 to 284 passing files
under the current default provider.

Pinned by `t/need-module-sees-its-own-imports.t` and
`t/eval-use-keeps-nativecall-ambients.t`, both of which pass under rakudo
2026.07 as well as mutsu.
