# `Signature.ACCEPTS` now honors a `where` constraint on a slurpy positional parameter

`Signature.ACCEPTS(Capture)` (and the equivalent `Capture ~~ Signature`
smart-match) delegated slurpy-positional handling
(`signature_accepts_value` in `src/runtime/seq_helpers/signature_helpers.rs`)
to a branch that only checked each remaining positional argument against the
slurpy parameter's `type_constraint`, element by element. It never consulted
`where_constraint` at all, so a signature like
`*@path where *[*-1].ends-with('.html')` accepted any array — the where
clause needs to run once against the whole assembled array (topicalized as
`$_`), the same way real call binding already did it
(`src/runtime/types/binding_signature.rs`).

The fix assembles the remaining positional arguments into a single array
`Value` and runs the existing `signature_where_ok` helper against it, mirroring
what call binding does.

## Effect

This was silently breaking `Cro::HTTP::Router`'s multi-candidate route
matcher, which calls `$handler.signature.ACCEPTS($cap)` directly to decide
whether to run a candidate handler. A route like

```raku
get -> 'content', *@path where *[*-1].ends-with('.html') { ... }
```

always reported a match (even for `/content/foo/bar.jpg`), so the router
invoked the handler body on a non-matching path instead of falling through
to a 404 — and the resulting mismatch between the router's `Capture` and the
handler body left the whole request unanswered, hanging
`t/http-router.rakutest` (vendored Cro::HTTP suite) indefinitely after test
180 of 360.

With the fix, `t/http-router.rakutest` completes without hanging: 355/360
pass (the remaining 5 failures are unrelated pre-existing bugs, now tracked
as `todo/tickets/named-parameter-user-subset-type-not-enforced-at-binding.md`
and `todo/tickets/request-body-pair-signature-match-picks-wrong-block.md`).

Pin: `t/signature-accepts-slurpy-where.t`.
