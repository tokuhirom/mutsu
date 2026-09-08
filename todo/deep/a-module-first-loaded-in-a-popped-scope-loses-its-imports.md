# A module first loaded inside a scope that unwinds loses its imports for good

When a module's **first** load happens inside a scope that is later rolled back —
an `EVAL`, or a `use`-containing block — the routines it and its transitive
dependencies imported are removed with that scope, while `loaded_modules` keeps
the module recorded as loaded. The subsequent genuine `use` therefore
short-circuits and cannot put the imports back, and the module is permanently
unable to resolve them.

rakudo disagrees in both shapes.

```raku
# tmp/nclib/Inner.rakumod
unit module Inner;
use NativeCall;
sub inner-probe() is export { defined(&nativecast) ?? 'visible' !! 'MISSING' }

# tmp/nclib/Outer.rakumod
unit module Outer;
use NativeCall;
use Inner;
sub outer-probe() is export { inner-probe() }
```

| script | rakudo | mutsu |
| --- | --- | --- |
| `use Outer; outer-probe()` | visible | visible |
| `{ use Outer; } use Outer; outer-probe()` | visible | **MISSING** |
| `EVAL 'use Outer; 1'; use Outer; outer-probe()` | visible | **MISSING** |

Note the failure is in `Inner`'s own view of `&nativecast` — the *nested*
module's import, not the importing script's.

## Two independent mechanisms

**1. The block shape — `pop_import_scope`'s registry retain.**
`pop_import_scope` keeps only `ks.contains("::") && !ks.starts_with("GLOBAL::")`
from `registry.functions`, i.e. it drops bare and `GLOBAL::`-qualified imported
aliases added since the push. `import_module` registers under
`target_pkg = self.current_package()`, and a `unit module`'s body runs with
`current_package()` at `GLOBAL` (see the comment at
`runtime_module_exports.rs:342`: "runtime registration used the GLOBAL
package"), so `Inner`'s `use NativeCall` lands as `GLOBAL::nativecast` and is
dropped with the enclosing block. Verified by disabling that retain: the block
row becomes `visible`, the EVAL row does not change.

**2. The EVAL shape — `eval_eval_string`'s registry rollback.**
`eval_eval_string` snapshots the routine registry and calls
`restore_routine_registry_eval` afterwards, which removes everything the EVAL'd
code registered — including a module's imports. `loaded_modules` is deliberately
never rolled back ("a module stays loaded forever, only its exported symbols are
lexically scoped"), so the two disagree: the module is loaded and its routines
are gone.

## Why it matters now

`Test`'s `use-ok` is `EVAL ( "use $code" )`, so **every** file that does

```raku
use-ok 'Some::Module';
use Some::Module;
```

hits mechanism 2 — and that is the shape the bundled-library suites use.
It is what fails the `Bundled-library test suites` gate on the vendored-`Test`
switch (PR #7523): 10 whitelisted files across `Cro::HTTP`, `DBIish`, `Digest`,
`NativeHelpers::Blob` and `NativeLibs` regress, with
`NativeHelpers::Blob t/01-basic.t` the smallest case:

```
Unknown function: nativecast
  in sub BODY_OF at lib/MoarVM/Guts/REPRs.pm6 line 64
```

Replacing that file's single `use-ok` line with `pass` makes it pass 24/24 under
the vendored provider, which is what identifies `use-ok` as the trigger. The
underlying bug is **provider-independent** — the repro table above loads no
`Test` at all — but mutsu's native `Test` provider does not implement `use-ok`
by EVALing, so only the vendored module exposes it.

## A measured partial fix, and the second layer it uncovers

Rolling `loaded_modules` back together with the routine registry — snapshot it
next to `snapshot_routine_registry()` in `eval_eval_string`, restore it next to
`restore_routine_registry_eval` — makes the module load for real on the later
`use`, and **fixes the EVAL row** (`visible`, matching rakudo). It does *not* fix
the battery case: the error merely moves on to

```
No such method 'realstart' for invocant of type 'Any'
  in sub carray-from-blob at lib/NativeHelpers/Blob.pm6 line 79
```

`BODY_OF` reads `%known-bodies{any.REPR}`, an `our` hash in
`MoarVM::Guts::REPRs`, and gets `Any` — so a module's *package state* does not
survive the reload either. Whatever the fix is, it has to leave a module loaded
inside an EVAL in ONE consistent state: either fully rolled back (registry,
`loaded_modules`, package globals, class/role registrations) so the later `use`
rebuilds all of it, or fully persistent so nothing needs rebuilding. Half of
each is what is broken today, and the partial fix only moves which half.

That is why this is a `deep/` item: it is a decision about EVAL's isolation
boundary, not a local repair. The partial change was measured and then reverted
rather than shipped, because a change to module-load semantics that still leaves
the headline consumer failing is not worth its blast radius.

### Which state rakudo implies: persistent, with lexical imports

Measured, so the choice above does not have to be guessed:

| after `EVAL 'use Outer; 1'` | rakudo |
| --- | --- |
| `EVAL 'outer-probe()'` (a fresh EVAL) | `X::Undeclared::Symbols` |
| `use Outer; outer-probe()` (the outer scope) | `visible` |
| `EVAL 'use Outer; outer-probe()'` (same EVAL) | `visible` |

So rakudo scopes the **imports** to whichever scope ran the `use` — a later,
unrelated EVAL cannot see them — while the **module and its own state stay
loaded process-wide**, which is what lets the outer scope's own `use` re-import
and work.

mutsu's `eval_eval_string` therefore rolls back the wrong thing. It discards the
module's *registrations*, which should persist, instead of scoping only the
*aliases the EVAL imported*, which is exactly what `pop_import_scope` already
does for a block and what its keep-rule ("a module's own fully-qualified source
definitions persist, because a sibling block's later `use` re-imports from
them") is built around. Rolling `loaded_modules` back — the partial fix above —
pushes in the opposite direction and is why the second layer (`our` package
state lost across the reload) appeared: it made the module reload when it should
never have been unloaded.

Target state, then: leave the module loaded and its registrations intact across
the EVAL, and scope only the imported aliases, so the later `use` re-imports
from the persistent definitions. Mechanism 1's `GLOBAL::`-qualified aliasing for
a `unit module`'s own imports is the remaining obstacle to that, since those
aliases are indistinguishable from a genuine GLOBAL import today.

## Reproducing

Write the two modules above under `tmp/nclib/`, then the three scripts, and
compare against `raku -Itmp/nclib`. For the real consumer:

```sh
MUTSU_BIN=target/release/mutsu scripts/battery-testsuite.sh   # the gate
cd tmp/battery-testsuite/NativeHelpers__Blob
MUTSU_REAL_TEST=1 ../../../target/release/mutsu -I lib t/01-basic.t
```

## Related

- PR #7523 (the vendored-`Test` default switch) is blocked on this.
- `news/2026-09/vendored-test-module-is-the-default-provider.md` — the campaign
  this surfaced in, and the pattern it follows: the native provider was hiding
  a general interpreter gap.
- `pop_import_scope` in `src/runtime/runtime_module.rs` carries the existing
  keep-rule that mechanism 1 would extend.
