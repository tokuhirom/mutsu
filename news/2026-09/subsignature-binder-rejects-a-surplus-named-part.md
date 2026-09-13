# A destructuring sub-signature naming only part of a named capture now rejects the rest

Found while fixing #8340. That ticket's first repro no longer failed the way
it originally reported, but it still diverged for a different reason:

```raku
if (a => 1) -> (:key($k)) { say $k }
```

`raku` refuses the bind: `Unexpected named argument 'value' passed in
sub-signature`. mutsu bound `$k` and silently dropped `value`.

A `Pair`'s capture has exactly two named parts, `key` and `value`.
`(:key($k))` names only one, so `value` is an unaccounted named argument and
rakudo rejects the call the same way it rejects a surplus named argument at
an ordinary call site. #8325 already implemented exactly this rule for the
*multi-candidate matcher* (`associative_entries_all_named` and the `Pair`
check beside it in `src/runtime/types/signature.rs`), so
`multi sub f(%h (:$x!))` correctly refuses to match `{:x(1), :y(2)}`. But a
plain (non-`multi`) call never consults that matcher — it reaches the
*binder* (`bind_sub_signature_from_value`) directly — and the binder had no
equivalent check, so the same rule was missing exactly where a
sub-signature is actually bound.

The fix mirrors the matcher's three shapes (a `Pair`/`ValuePair`, an ordinary
`Capture`, and an all-named `Hash`/`Map` destructure) inside the binder, and
reuses `consumed_named_keys` — already computed there to feed a named slurpy
— to decide which keys are accounted for, rather than re-deriving that from
scratch. Reusing it turned out to matter: a first version that checked each
sub-param's own name directly missed a `:outer(:$inner)` NAMED ALIAS's inner
name (`destructure-named-alias.t`'s `k({throw => True})` case, where `:die(:$throw)`
must accept the call under either spelling), which `consumed_named_keys`
already accounts for. It also had to learn that a bare `|` capture (unlike a
typed `*@rest`, which does **not** exempt a named surplus — verified against
`raku`) swallows everything, named arguments included; missing that broke
`pointy-anon-destructure-later-param.t`'s `|`-tailed shape. Both were caught
by running every `t/` file that touches sub-signature destructuring before
landing, per the ticket's own "worth checking `t/` ... before landing" scope
note.

`t/routines/signature/given-with-destructuring-pointy-param.t` gains the
`if`/Hash cases the ticket named, plus the `|`-capture regression guard.

[#8357](https://github.com/tokuhirom/mutsu/issues/8357)
