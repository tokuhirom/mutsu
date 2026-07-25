# A `unit role` body's `use` now loads before the role composes

```raku
unit role R;

use Base;
also does Base;
```

failed with `Unknown role: Base` — **even when `Base` existed and loaded fine on
its own**. Every role in the `PDF::Class` distribution is written this way, which
is what the real-dist compatibility sweep had recorded as
`PDF::AcroForm: Unknown role: PDF::COS::Tie::Hash`.

## Root cause

`use` is compile-time in Raku, so the module is loaded before the enclosing
package's traits are applied. In mutsu a unit-role body runs at
*role-registration* time, and registration walks the body statements — processing
the `DoesDecl` that `also does Base` lowers to — before any `Stmt::Use` in that
same body has executed. So composition looked up a role that had not been loaded
yet.

The `unit class` form already had the fix: `parser/stmt/stmtlist.rs` hoists
`use`/`need`/`import` out of the captured unit-class body to just before the
declaration when the body declares parents (added for
`roast/integration/diamond.t`). The `unit role` arm right below it did not. It
does now, gated the same way — only when the body actually composes something
(`DoesDecl` present), so the hoist stays as narrow as the class one. Hoisted
statements keep the same compilation-unit scope, so imported symbols remain
visible.

Role registration already knew about this ordering and worked around it
elsewhere: the parameter type-constraint validation accepts an unresolvable
qualified type when a body `use` could supply it, with the comment "the body's
`use` runs after this validation". That workaround stays; composition needed the
module for real, not a deferral.

## Side effect: honest missing-dependency errors

Because the `use` now runs first, a role body that imports a module which is
genuinely absent reports the real cause instead of a downstream symptom:

```
# before: Unknown role: PDF::COS::Tie::Hash
# after:  Could not find PDF::COS::Tie::Hash in: ...   (matches raku)
```

That reclassifies `PDF::Class` in the sweep from `runtime_error` (apparent mutsu
bug) to `missing_dep` (its `PDF::COS` dependency is simply not installed — raku
cannot load it here either).

Checking the rest of that sweep's failures the same way — against `raku -I lib` —
found that **4 of the 6 "real mutsu failures" were not mutsu bugs**:

- `Qwiratry::Test` — its `parse_error` is an undeclared custom Unicode operator
  (`↱`) supplied by the absent `Qwiratry::Query::Slang`; raku fails on the same
  missing dependency.
- `RakudoContainerfileBuilder` — loads fine, but exports a `MAIN`, which
  `-e 'use M'` then dispatches; it prints its usage and exits non-zero. raku does
  exactly the same (exit 2).
- `Raku::Pod::Render` — same shape: its `InstallAtomHighlighter` exports a `MAIN`
  that shells out to `npm`/`git`, which hangs under the sweep's no-net sandbox.

Only `String::Utils` (which needs an `nqp::` op layer,
`todo/deep/nqp-op-layer-missing.md`) and this role bug were mutsu's. PLAN §B4 now
records both the corrected counts and two follow-ups: bucket an
exported-`MAIN` dispatch separately in `scripts/dist-compat-sweep.py`, and always
confirm a non-`missing_dep` bucket against raku before believing it.

Pinned by `t/unit-role-body-use-before-also-does.t` (6 subtests: a method from the
composed parent, the composing role's own method, the type hierarchy checked by
name so the test does not import — and thereby pre-load — the parent, two hoisted
`use`s composing two parents, and the `unit class` counterpart as a symmetry
guard). Fixtures: `t/lib/UnitRole{Base,Second,Composer,TwoParents}.rakumod`,
`t/lib/UnitClassComposer.rakumod`. All 6 identical under raku.
