# A module first loaded inside an `EVAL` keeps its imports

A module whose **first** load happened inside an `EVAL` lost the routines it and
its transitive dependencies had imported, while `loaded_modules` kept it recorded
as loaded — so the subsequent genuine `use` short-circuited and could not put
them back, and the module was permanently unable to resolve them.

```raku
# Inner.rakumod:  unit module Inner; use NativeCall;
#                 sub inner-probe() is export { defined(&nativecast) ?? 'visible' !! 'MISSING' }
# Outer.rakumod:  unit module Outer; use NativeCall; use Inner;
#                 sub outer-probe() is export { inner-probe() }

use MONKEY-SEE-NO-EVAL;
EVAL 'use Outer; 1';
use Outer;
say outer-probe();     # rakudo: visible    mutsu, before: MISSING
```

What went missing is `Inner`'s own view of `&nativecast` — the *nested* module's
import, not the importing script's.

## Why the interpreter was in two minds

`eval_eval_string` snapshots the routine registry and restores it afterwards, so
everything the EVAL'd code registered is rolled back. `reinstate_module_functions`
already exists to exempt a module's own routines from that rollback, keyed off
`module_registered_functions` — but the set was collected with
`!ks.starts_with("GLOBAL::")`, and a `unit module`'s body runs at
`current_package() == GLOBAL`, so `Inner`'s own `use NativeCall` registered
`GLOBAL::nativecast` and was excluded. `loaded_modules` is deliberately never
rolled back, so the two halves disagreed: loaded, with its imports gone.

## What rakudo actually does, measured

The fix direction was measured rather than guessed. After `EVAL 'use Outer; 1'`:

| | rakudo |
| --- | --- |
| `EVAL 'outer-probe()'` (a fresh EVAL) | `X::Undeclared::Symbols` |
| `use Outer; outer-probe()` (the outer scope) | `visible` |
| `EVAL 'use Outer; outer-probe()'` (same EVAL) | `visible` |

So rakudo scopes the **imports** to whichever scope ran the `use`, while the
**module and its own state persist process-wide** — which is what lets a later
`use` elsewhere re-import and work.

## The fix

The `GLOBAL::` exclusion is dropped from the `module_registered_functions`
delta. The delta is taken *before* `import_module`, and that timing already
separates the two kinds of alias exactly along rakudo's line:

- an alias installed while the module's **own body** ran (its `use NativeCall`,
  its `use Inner`) is in the delta. It is lexical to *that module*, which stays
  loaded, so it now survives whatever scope the load happened to sit inside.
- an alias `import_module` installs for the **importing scope** is added after
  the delta and stays excluded, so it remains lexical to that scope:
  `{ use Foo } EVAL('foo()')` still dies (`roast/S11-modules/lexical.t` passes
  unchanged).

An earlier attempt — rolling `loaded_modules` back alongside the registry — was
measured and rejected: it fixed the repro but only moved the bundled-library
failure on to `No such method 'realstart' for invocant of type 'Any'`, because it
made the module *reload* when rakudo says it should never have been unloaded.

## What it unblocked

`Test`'s `use-ok` is `EVAL ( "use $code" )`, so every file shaped like

```raku
use-ok 'Some::Module';
use Some::Module;
```

hit this — which is the shape the bundled-library suites use. It was the
remaining blocker on the vendored-`Test` default switch
(`news/2026-09/vendored-test-module-is-the-default-provider.md`): the
`Bundled-library test suites` gate reported 10 regressed whitelisted files. The
smallest, `NativeHelpers::Blob t/01-basic.t`, died at test 4 with

```
Unknown function: nativecast
  in sub BODY_OF at lib/MoarVM/Guts/REPRs.pm6 line 64
```

and now passes 24/24 under both providers. Locally the gate goes from 276/312
with 15 regressions to 284/312 with 6 — every remaining one a `DBIish` MySQL
test that needs a live server and fails identically without this change.

The bug was provider-independent; mutsu's native `Test` provider simply does not
implement `use-ok` by EVALing, so only the vendored module exposed it — the same
pattern as the other gaps that campaign uncovered.

Pinned by `t/module-loaded-in-eval-keeps-imports.t`.

## Still open: the same shape with a plain block

`{ use Outer; } use Outer; outer-probe()` still answers `MISSING` where rakudo
answers `visible`. That is a second, independent mechanism —
`pop_import_scope`'s own `registry.functions` retain, which applies the identical
`!starts_with("GLOBAL::")` rule to a block's exit and would need the same
module-owned carve-out. It is filed as
`todo/tickets/block-scoped-use-drops-a-nested-modules-imports.md`; nothing in the
bundled libraries or roast depends on it today, and it is a smaller, separable
change than the EVAL half.
