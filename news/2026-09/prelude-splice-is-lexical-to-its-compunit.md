# A prelude splice is lexical to the compunits it was spliced into

`use`ing a module that itself uses the native-call module left that module's
helpers — `nativecast`, `nativesizeof`, `cglobal`, `explicitly-manage`,
`refresh` — declared in the *using* scope, two levels up, where rakudo leaves
them undeclared. GH #7612 tracked it as the flat routine registry leaking a
nested module's imports; measuring it first showed the leak is narrower and has
a different cause.

## What actually leaked

An ordinary nested import of a **routine** does not leak. With three plain user
modules — a leaf exporting `sub leafy`, a middle module that `use`s it, and a
script that `use`s only the middle one — mutsu and rakudo agree exactly: `leafy`
is undeclared in the script, and the middle module's own routines still resolve
it. The flat registry is already scoped correctly there, by the
`module_registered_functions` delta that #7580 introduced. (An imported
*variable* or *class* is a different story — see "What is still open" below.)

What leaked was the **prelude splice**. mutsu has no `NativeCall.rakumod`; the
five helper routines are injected as an `our sub` prelude into every compunit
whose source mentions the module (`inject_nativecall_subs_prelude`), and each
registers under `GLOBAL::` rather than under the host compunit's package. That
`GLOBAL::` registration is deliberate and still necessary — a method body
running under *any* package has to be able to call the helper by bare name,
which is the shape `NativeHelpers::Pointer` has, and registering under the host
package instead left those bodies unable to see it. But a process-global
registration is also process-global *visibility*: once any compunit anywhere had
pulled the prelude in, `&nativecast` resolved from every scope in the process.

The issue's own repro is exactly this shape — `BlockUseNestedOuter` →
`BlockUseNestedInner` → the native-call module — which is why it reproduced with
no block anywhere, and why it did not reproduce with ordinary user modules.

## The fix

Keep the registration process-global; make the *visibility* lexical.

`prelude_declaring_units` records, per prelude key, the compilation units the
declaration was actually spliced into (`?FILE` at registration time, normalized
so the main script is `main_unit()`). A splice is per compunit and idempotent —
the first registration wins and later ones return `Unchanged` — so this set is
what records the later ones, which the routine registry alone cannot.

Routine resolution then consults the compunit that is *executing*:
`executing_unit_sym()` is the allocation-free `Symbol` form of the existing
`executing_source_file()` walk (innermost frame's `def_file`, skipping inlined
bare blocks, falling back to `?FILE`), and `prelude_visible_here` answers
whether a resolved key is reachable from there. `resolve_function` skips a
prelude key that is not, and `has_declared_function` agrees with it.

This is the "consult the compilation unit a routine is being resolved from"
model the issue asked for, applied at the one place the leak actually lives. It
costs nothing on the normal path: every non-prelude key answers `true` without
touching the map, and the whole check short-circuits on the empty prelude set,
which is every program that does not use the native-call module.

The result matches rakudo on both edges: the using scope no longer sees the
helper, and the declaring module's own routines still do — including through an
enclosing block's import-scope pop, which is the direction #7580 fixed.

## What is still open

Two genuine flat-namespace leaks remain, both measured against rakudo while
investigating this, and both a different shape from the prelude one: a nested
module's imported `our ... is export` **variable**, and its imported **class**,
are still reachable from the using scope. Fixing those needs module-scoped
variable and type resolution to work for a `unit module`'s own subs, which
`lookup_in_running_package` cannot do today because it keys on the running
frame's *package* — GLOBAL for a unit module's subs — rather than on its
compunit. Filed separately, with the repro, as #7692.

## Tests

- `t/nested-module-native-prelude-not-visible-to-user.t` — the full shape, in
  both directions, plain and block-scoped. Every helper name is assembled at
  runtime: the splice gate is a source-text check over the file's *code*
  (comments and Pod are stripped first), so spelling one plainly would inject
  the prelude into the test compunit and legitimately declare it.
- `t/block-use-keeps-nested-module-imports.t` — its `todo`-note for this
  divergence is now an assertion.
