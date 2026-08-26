# `but`-mixing a role onto a list no longer breaks its `Positional` binding

`my @positional := <a b> but R` died with `Type check failed in binding; expected Positional but
got List`, even though `($x ~~ Positional)` on the very same value answered `True`.

## Root cause

Two different oracles answered "is this Positional?", and only one of them knew about mixins.
`Interpreter::type_matches_value` has a `ValueView::Mixin` arm that recurses into the wrapped
value, so the smart-match said `True`. The `@`-sigil bind check,
`Interpreter::bind_positional_value` (`src/vm/vm_var_assign_set_local.rs`), is a flat
`match decontained_popped.view()` over `Array | LazyList | Seq | Slip | Range | … | Instance |
Package` — with no `Mixin` arm at all, so a role-mixed list fell to the `_ => false` arm. This is
the same "tested `ValueView::Instance`, forgot `ValueView::Mixin`" shape that was independently
breaking string coercion (see
[array-but-role-mixin-name-suffix-and-join-str.md](array-but-role-mixin-name-suffix-and-join-str.md)).

## Fix

`bind_positional_value` now unwraps a `Mixin` to its inner value before the type match — a
composition can only *add* to what the wrapped value does — and additionally accepts the bind
outright when one of the composed roles is `Positional` itself, which is what makes the newly
composable `%h but Positional` bind to a `%`/`@` target.

## Residual divergence (separate, pre-existing)

`<a b> but R` reports `Array+{R}` where raku reports `List+{R}`: mutsu evaluates the `<...>`
word-list literal to an `Array`, not a `List`. That is a list-literal typing gap unrelated to
mixins — the binding itself, the element access, and `~~ Positional` all now behave correctly.

Pinned by `t/role-mixin-survival.t`.
