# `self (-) %allowed` parses, and a role-mixed hash coerces to a Set by its keys

Working `Hash::Restricted` 0.0.9 (`ecosystem/dists/H/Hash--Restricted~9935eb4e.json`, `red`, dying
at its first assertion with `Unknown prefix operator: (-)`) turned up two general interpreter gaps.
Neither is about restricted hashes.

## A nullary term keyword is not a listop head

`self`, `now`, `time`, `pi`, `Any`, `Mu` and the rest of the nullary term keywords are *terms* in
rakudo — `term:sym<self>` and friends — so what follows one of them is an **operator** position.
mutsu's no-paren listop branch in `src/parser/primary/ident/identifier_call.rs` did not know that: it
accepts any identifier followed by whitespace and a term start as a call, and `(` is a term start. So
`self (-) %allowed` compiled to `self((-) %allowed)` and died at runtime with `Unknown prefix
operator: (-)`, and outside a module the same source got the compile-time `Undeclared routine: self`
instead. `Mu` took the same path.

The branch now refuses a term keyword as a listop head unless a routine of that name is actually
declared or imported, which is the rule the rest of that branch already follows. `self (-) $set` is
set-difference, and `self (1, 2)` is the "two terms in a row" error rakudo reports rather than a
call.

`now` and `time` had a second, narrower version of the same bug in
`src/parser/primary/ident/term_literals.rs`: both correctly reject the call form `now(...)` as
rakudo's `Undeclared routine`, but they tested for the paren *after* `trim_start()`, so
`now (-) $set` was rejected too. Only an adjacent paren is the call form; after whitespace the
parenthesis belongs to the operator position.

## QuantHash coercions fold through a role mixin

A role mixin **wraps** a value without replacing it: rakudo's `%h does R` is a `Hash+{R}`, still a
Hash, and `@a but R` is still an Array. Every QuantHash coercion in mutsu matched on
`value.view()` without stripping `ValueView::Mixin`, so a mixin fell through to the "unknown scalar"
arm and contributed **the whole hash as one element**: `%m.Set` answered
`Set.new({:a(42), :b(0)})`, `%m (-) $set` cancelled nothing, `%m (.) $bag` was empty, and
`'a' (elem) %m` was False.

`runtime::utils::quanthash_operand` (descalarize + strip the mixin) and its mixin-only half
`strip_quanthash_mixin` (for nested element positions, where stripping the `Scalar` container would
change the flattening rule) are now applied at every coercion entry point: `coerce_to_set`,
`coerce_value_to_quanthash`, `to_bag_map`, `to_mix_map`, `set_type_level`, `builtins`' `to_set` /
`to_bag` / `to_mix`, the VM's `value_to_set_keys` / `value_to_bag_counts` / `value_to_mix_weights` /
`set_contains` / `coerce_to_bag` / `coerce_to_mix` / `set_type_level_full`, and
`ops_set`'s four `apply_set_*` entry points. `.Set`/`.Bag`/`.Mix`/`.SetHash`/`.BagHash`/`.MixHash`
and `(-)`/`(|)`/`(&)`/`(^)`/`(+)`/`(.)`/`(elem)` all agree with rakudo on a mixin now.

Only a **role** mixin is folded through. An allomorph is a `Mixin` too — `<1>` is
`Mixin(Int(1), {Str => "1"})` — and its whole point is to be a distinct element from the value it
wraps, so `(1, "1", 1.0, <1>).Set` has four elements. The discriminator is the `__mutsu_role__<name>`
marker, the same one `dispatch_mixin_method_call`'s `.clone` arm already uses. The nested-element
form splits one step further: a role-mixed **aggregate** element flattens its contents in list
context (`(1, %h).Set` is `Set(1, "a")`), while a role-mixed **scalar** element keeps its own
identity (`(5, 5 but R).Set` has two elements, keyed `Int` and `Int+{R}`).

Pinned by `t/lang/term-keyword-not-listop-head.t` and
`t/collections/set-bag-mix/quanthash-folding-of-wrapped-values.t`, whose last five assertions are
exactly the identity cases above.

## What is left of the distribution

`Hash::Restricted`'s suite goes from 1 to 4 of 32 assertions. Assertion 5 onwards needs
[#8026](https://github.com/tokuhirom/mutsu/issues/8026): a role mixed into a non-`Instance` value has
no store for its own attributes, so the `%!allowed` key set the module's `STORE` computes is
discarded before `AT-KEY` can read it. Since ADR-0019 Phase 3 Stage 2c every `$!attr` access inside a
compiled method is cell-direct, and `self_instance_attrs` has no cell to offer for a `Mixin` over a
Hash — giving it one is a change to the `Value::Mixin` representation, so it is filed rather than
fixed here.
