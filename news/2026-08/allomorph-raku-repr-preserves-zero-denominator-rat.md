# Allomorphic `.raku` preserves zero-denominator Rats

`raku_value` had a representation-only inconsistency: direct `Rat.raku` used
the exact `<numerator/0>` form, while recursive rendering converted an inner
zero-denominator `Rat` to `Inf`, `-Inf`, or `NaN`. That caused `RatStr.new`
and padded fraction quote-words to lose their rational form.

The common recursive renderer now emits `<n/0>` for `Rat` and `BigRat` values
with a zero denominator. This keeps allomorphs and collections consistent with
direct `Rat.raku`. The regression test covers positive, negative, and zero
numerators, a padded angle-word allomorph, and nested list rendering.
