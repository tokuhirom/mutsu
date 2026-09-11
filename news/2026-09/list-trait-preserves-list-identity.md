# `is List` preserves the declared container identity

An `@` variable declared with `is List` was backed by an `Array` even though
mutsu already enforced the List's immutable assignment behavior. The variable
now retags its existing backing store as a List when the variable trait is
applied, so `.^name`, `.raku`, `is-deeply`, and mutation errors agree with
Rakudo.

Pinned by `t/oo/trait/list-trait-readonly.t` and `roast/S32-list/create.t`.

Closes #7882.
