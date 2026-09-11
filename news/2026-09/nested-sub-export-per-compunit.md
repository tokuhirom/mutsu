# `sub EXPORT` is scoped per compunit again

A module that both `use`s another `sub EXPORT` module **and** declares its own
hook failed to load at all:

```
$ mutsu -I lib -e 'use Outer; say outer-fn();'
Redeclaration of routine 'EXPORT'. Did you mean to declare a multi-sub?
  in block <unit> at lib/Inner.rakumod line 1
```

Raku scopes `sub EXPORT` to its compunit: each file may declare exactly one, and
two files' hooks never see each other. mutsu did not hold that invariant on the
run-time side. A module body runs under `GLOBAL`, so every module's `sub EXPORT`
registered under the single `GLOBAL::EXPORT` key, and sub declarations are
hoisted — entering the outer module's body registered `GLOBAL::EXPORT` before
its `use` of the inner module ran, so the inner module's own hoisted declaration
hit the redeclaration check in `registration_sub.rs` and killed the load.

Two hooks side by side worked, because the first is consumed and recorded in
`module_export_defs` before the second module loads. Nesting is what made the
two live at once, and it did not need them to share a file or a scope kind: a
`my sub EXPORT` in the outer module, or a three-deep chain, collided the same
way.

## The fix

`load_module` now hides whatever `EXPORT` routine an enclosing compunit
registered for the duration of a nested load
(`Interpreter::hide_export_routines`), so the nested module's own declaration
lands on a clean key, and restores it once `apply_module_export` has called and
dropped that module's hook.

The mechanism is the one `hide_toplevel_global_routines` already uses for
ordinary package-less top-level routines — but it deliberately could **not** be
folded into it, and the reason is the whole subtlety. That restore runs *before*
`apply_module_export`, so including `EXPORT` in it put an outer module's stale
hook back over the inner module's fresh one before it was ever read, which is
exactly why `is_toplevel_global_routine_key` excludes `EXPORT` today. The new
pair brackets `apply_module_export` instead, on both the success and the
error path, so an aborted load cannot leave the enclosing hook hidden.

Neither of the two paths the old comments warn about moves: the inherited
`&EXPORT` handoff (the Slangify pattern) is keyed by module in
`pending_inner_export_subs`, and the env discipline that keeps a bareword
package bound while the hook runs is untouched.

## Why it mattered

Ten distributions in the ecosystem ledger were `blocked_load` on this one root
cause — the largest single entry in that bucket, found by triage of the first
full-corpus sweep: `Data::Generators`, `Identity::Utils`, `List::AllUtils`,
`List::SomeUtils`, `PURL`, `SBOM::CycloneDX`, `span`, `Test::Async`,
`User::Timezone`, `UserTimezone`. It is the dominant lizmat idiom: a small
distribution that re-exports a dependency under a different name, with both ends
computing their exports in `sub EXPORT`. `Test::Async` is the one that reaches
furthest — it is a test framework, so every distribution that tests with it
inherited the failure.

Pinned by `t/modules/import-export/nested-export-sub-scoping.t`, which
covers the nested chain, the `my sub EXPORT` variant, three levels deep, an
outer hook that consumes the end user's `use` arguments, and the side-by-side
shape that already worked. All six assertions were measured against `raku`
first.
