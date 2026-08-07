# ADR-0019 D3-5: augment class method drift fixed

`augment class`'s `MethodDecl` arm (`registration_class_augment.rs`) diverged from the class and
role body walkers in ways that were user-visible, confirmed against `raku`:

- `augment class Foo { my method secret {...} }` put `secret` into `Foo`'s method table, so
  `Foo.can('secret')` wrongly reported it. `raku` keeps a `my method` out of the method table
  entirely — it is only callable lexically from a sibling declaration in the same block.
- `augment class Foo { our method pkg {...} }` made `pkg` directly callable as `Foo.new.pkg`, and
  did not register it as the package-qualified sub `Foo::pkg(invocant)` that `raku` expects.
- A public `method foo` and a private `method !foo` declared across a `class`/`augment class` pair
  wrongly collided ("already has a method 'foo'") — `raku` treats a method's privacy as part of
  its identity, so a public and a private method of the same name coexist in separate namespaces.

The fix mirrors the class walker: `MethodDef.is_my` now stores `is_submethod` (not the raw parser
`is_my` flag), `is_lexical_only`/`is_our_only` gating excludes `my method`/`our method` from the
method table, the `our`/`my` function forms are registered the same way the class walker registers
them, and duplicate-method detection compares `is_private` before rejecting a redeclaration. All
three regressions are pinned by the new `t/augment-method-lexical-scoping.t`, verified line-for-line
against `raku`.

This closes ADR-0019 D3-5, one of the reconciliation slices the D3 scoping pass and D3-2/D3-3/D3-4
set up by giving all three method-declaration walkers (class, role, augment) a shared
`CompiledMethodDecl` constructor: with drift expressed as unused struct fields instead of absent
destructure bindings, it became directly comparable and fixable at each walker's call site.
Remaining `augment_class` gaps (`handles` forwarders, custom traits, `is export`, BUILD/TWEAK
`:$!attr` validation) are documented in the ADR as still open, independently-scoped follow-ups.
