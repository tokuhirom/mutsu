# `but role` on a List reported Array, and hid Array elements from a slurpy

Found by the 2026-09-27 doc-diff sweep (issue #9787).

**`(<a b> but R).^name` reported `Array+{R}`, not `List+{R}`.** Composing a
role onto a value builds its `.^name` from `value/types.rs`'s
`what_type_name`, which collapses every non-itemized `ArrayKind` (`List` and
`Array` alike) to `"Array"`. That helper is also used for other `Mixin`
bases — notably `Package`, whose real name it resolves through
`user_facing_type_name` — so the fix narrows to the one wrong case (a
non-real-Array `ArrayKind`) rather than swapping in the stricter
`runtime::utils::value_type_name` wholesale, which would have regressed
type-object mixin names back to the generic `"Package"`.

**`join(">", @o but role { method Str {...} })` treated the whole Array as
one element.** `join`'s slurpy pre-render step
(`join_prerender_user_stringifier`) stringified any `Mixin` value through its
role's `.Str` before the slurpy had a chance to flatten it, so a `but`-mixed
real Array collapsed to the role's single string instead of joining its
elements. `flat_val` already flattens a container-inner mixin through its
inner value — the mixin's own `Str` override applies to the value as a
whole, not to the elements a flattening slurpy exposes instead of it — so
the pre-render step now recurses into the inner container the same way,
only falling back to the mixin's `Str` when the inner value is not itself a
flattening container.

Pinned by `t/oo/but-role-on-list-array-name-and-slurpy.t`.
