# `my %h is CustomTrait = initializer` now re-feeds a mixed-in role's `STORE` at declaration time

For `my %h is CustomTrait = initializer`, where `CustomTrait` is a
user-declared *trait name* dispatched through a
`multi sub trait_mod:<is>(Variable:D \v, ...)` (not itself a registered
class/role name) that mixes in a role with a `STORE` method — the
`Hash::Restricted` / `Injector` idiom, where the trait handler calls
`trait_mod:<does>(v, R)` to tie the declared variable to a role — mutsu used
to compile/execute the initializer's plain store *before* the trait
application ran. By the time the role's `STORE` override could see the
data, `%h` was already populated via a raw, non-dispatched write, so the
role's `STORE` never ran at all for the declaration's own initializer. Real
Raku dispatches the declaration assignment through `STORE` with
`:INITIALIZE`, which several dists (`Hash::Restricted`'s `%!allowed`
auto-population being the motivating case) depend on.

`vm_var_trait_ops.rs`'s `exec_apply_var_trait_op` already had a working
compensation for the narrower `is ClassName` form (`my %h is
SomeRoleWithStore = ...`, where `SomeRoleWithStore` is itself a registered
class/role name): it gathers whatever the initializer's `SetLocal` already
stored, builds the class/role instance, binds it to the variable, then
re-feeds the gathered initializer value through `STORE` with
`:INITIALIZE`. That mechanism was gated on the trait name itself being a
registered class/role, which a custom trait name like `"restricted"` never
is — the mixin happens indirectly, from *inside* the `trait_mod:<is>`
handler.

The fix generalizes the same re-feed to the generic `trait_mod:<is>`
dispatch path: the declaration's raw initializer value is captured before
any trait dispatch runs (mirroring the existing `is ClassName` block), and
after `trait_mod:<is>` returns and any `trait_mod:<does>` writeback is
applied, if the value now bound to the variable is a `Mixin` whose composed
role(s) declare a public `STORE` method, that raw value is re-fed through
`STORE` with `:INITIALIZE`. The underlying container was already correctly
populated by the compiler's plain `SetLocal`, so the STORE result is only
written back when it is itself tie-bindable (mirroring
`tied_store_dispatch`'s own fallback) — a `STORE` that merely `callsame`s
into an untracked default (returning `Nil`, since a plain `Hash` has no
compiled `STORE` in its MRO for `callsame` to fall through to) leaves the
already-correct value untouched.

Verified against `raku` with the ticket's repro (a role `R` with a `STORE`
that prints and `callsame`s, mixed in via a custom `is restricted` trait):
mutsu now prints `R.STORE called with (:a(42), :b(666))` at declaration
time, matching real Raku. `t/custom-var-trait-store-initializer.t` pins
this case, the already-working `is ClassName` case, and a custom trait that
mixes in a role *without* `STORE` (confirming it stays unaffected). Full
`make test` (3308 files, 30735 tests) shows no regressions from this
change.
