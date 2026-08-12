# A named `:$param`'s declared type constraint is now enforced at binding

`bind_function_args_values` (`src/runtime/types/binding_signature.rs`) has
one big `for pd in param_defs` loop with separate arms per parameter kind.
The positional scalar arm ran a ~230-line type-check-and-coerce block
(built-in types, coercion types like `Str(Numeric:D)`, `:D`/`:U` smiley
diagnostics, `&`-sigil Callable-return-type checking, Num-widening,
Associative/Hash coercion, and user `subset` `where`-clause enforcement) —
but the named arm (a separate ~300-line block later in the same loop)
resolved the matching `Pair` argument and bound it directly, **without ever
calling into that check**. Any named parameter's declared type — built-in
(`Int`), user class, or user `subset` alike — was silently ignored:

```raku
sub f(Int :$x!) { "ok $x" }
say f(x => "not an int");   # raku: throws; mutsu (before this fix): "ok not an int"
```

Fixed by extracting the type-check-and-coerce block into a shared method,
`check_and_coerce_param_type`, called from both the positional arm (as
before) and the named arm's "found" case, checked against the raw value the
caller passed before any container-sharing/`rw` promotion.

## A pre-existing default-value bug this surfaced

Enabling the check exposed a second, unrelated bug: an **unsupplied**
`&`-sigil named parameter's implicit nominal type defaulted to `Any`
instead of `Callable` (`missing_optional_param_value`,
`src/runtime/types/mod.rs`), so `&cb ~~ Callable` was `False` for an
unbound `:&cb`. This broke `Template::Mustache` (a bundled battery):
`Logger.new(:routine(&log-routine))` passes an unsupplied `TWEAK(:&log-
routine)` into a `Callable :$routine` constructor param, which the newly
strict named-param check correctly rejected — `Any` does not smart-match
`Callable`. Fixed the default itself: an unconstrained `&`-sigil param's
missing-value fallback is now the `Callable` type object, matching real
Raku (`Callable[T]` for a `T`-return-constrained `&`-sigil param was left
as a known follow-up — not exercised by anything in this repo yet).

## Effect

`t/http-router.rakutest` (vendored Cro::HTTP suite, part of the ongoing Cro
compatibility campaign): 355/360 → 359/360 (the 4 newly-passing subtests
are "Non-matching (optional) unpack gives 400 error (subset, Str/Int)" —
Cro::HTTP::Router's `is query` parameter binding now correctly throws
`X::TypeCheck::Binding::Parameter` for a mismatched value, which the router
catches and turns into an HTTP 400). Cro::HTTP suite-wide: 30/34 → 31/34
fully-green files.

Pins: `t/named-param-type-constraint-enforced.t`,
`t/mustache-battery.t` (regression guard for the `&`-sigil default fix).
