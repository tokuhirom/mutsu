# Runtime trait_mod:<is> writeback and private-stub composition fixes (#8806)

Fixed the two remaining bugs (of three found while investigating #8806) that
made a real-world AttrX::Lazy-shaped custom `trait_mod:<is>` handler behave
differently depending on whether the module declaring it was `use`d at the
top level or from inside a routine at runtime — dying with `Can't use
unknown trait 'is' -> 'lazy'` only on the runtime-use path. This blocks
`Math::Fitting`'s multidimensional tests via its `Math::Matrix` dependency.
The third bug (role attribute defaults not seeing `self` correctly during
runtime mixin composition) was fixed separately and independently in
`news/2026-09/ecosystem-multi-dispatch-and-role-defaults.md`.

## A role's private stub method is never a composition requirement

`method !foo { ... }` inside a role is not a compile-time "must be
implemented" requirement, unlike a public stub — private methods are not
virtual in Raku (`self!foo` always resolves to the role's own `!foo`, never
to a composing class's method of the same name), so there is nothing for a
class to "implement". Verified against `raku`: composing a role with an
unimplemented private stub raises no error, and only dies "Stub code
executed" if the role's own body ever calls it. `Math::Matrix::Util` (a real
ecosystem dependency) relies on this: it stubs a private `!clone-rows` that
`Math::Matrix` never implements and the role's own body never calls, which
used to make `Math::Matrix` fail to compose at all — and, combined with
mutsu's forward-reference class hoisting, made a class composing such a role
run its whole body TWICE (the hoisted attempt's failure being silently
tolerated, followed by the real one).

## The `trait_mod:<is>` writeback slot conflated two different values

`apply_attribute_traits` (the `has $.x is some-trait` dispatcher) relies on a
single `trait_mod_writeback_key`/`trait_mod_writeback_value` relay to learn
what a custom `trait_mod:<is>` handler did with `$attr` when the handler
performs a role mixin. AttrX::Lazy's real handler mixes into *two* different
targets in one call — `$attr does LazyAttribute;` and then, separately,
`$class.HOW does LazyAttributeContainerHOW;` so its `compose` hook can
install the lazy accessor — and the single slot let the second (HOW) mixin's
result silently overwrite the first (attribute) mixin's result. The
attribute's own composed value was then never cached as its `^attributes`
meta-object; worse, when the double class-body run from the private-stub bug
above happened, the *wrong* cached value resurfaced on the second run and no
longer type-checked as `Attribute:D`, which is what actually produced the
reported error on a runtime `use` but not a top-level one (a top-level
`use`'s class body only ever runs once).

Fixed by tracking the attribute's own resulting mixin in a second,
independent field (`trait_mod_attr_writeback_value`), populated by the same
`does`/`but`/assignment sites but skipped for a mixin that already persists
through its own dedicated path (`eval_does_values`'s `how_target_from_value`
branch, which writes `registry.class_how_values`). The original
`trait_mod_writeback_value` is untouched and keeps serving the `compose`-hook
detection it always has, on whichever `does` ran last.

With both fixed, `sub load { use Math::Matrix; "loaded" }; say load()` runs
identically to the top-level `use Math::Matrix; say "loaded"` — both now
reach the same, separate, still-open `Math::Matrix::Util` private-method
dispatch gap (`No such private method 'check-index'`) rather than diverging
at the attribute-trait stage. Getting `Math::Matrix`'s lazy accessors to
actually compute correct values additionally needs mutsu to invoke a
`compose` hook mixed into `$class.HOW`'s effects for real (`type.^add_method`
inside it does not yet take effect) — a further, separate gap, not attempted
here.

Regression tests: `t/oo/role/role-private-stub-not-required.t` and
`t/oo/attribute/attr-trait-runtime-use-lazy-style.t` (using
`t/lib/RuntimeUseLazy*.rakumod` fixtures that reproduce AttrX::Lazy's exact
dual-mixin shape via a genuine runtime `use`).
