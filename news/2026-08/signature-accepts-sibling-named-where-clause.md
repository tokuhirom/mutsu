# `Signature.ACCEPTS` now sees sibling named params inside a `where` clause

A destructuring signature's `where` clause can reference a *sibling*
parameter by name, e.g. `-> (:$x, :$y where $y > $x) {...}` — this is
exactly the mechanism `Cro::HTTP::Router`'s `request-body` uses to pick
between candidate handler blocks by signature match
(`$handler.signature.ACCEPTS(\(body))`).

`signature_accepts_value` (`src/runtime/seq_helpers/signature_helpers.rs`)
checked each parameter's `where` clause via `signature_where_ok`, which
only binds `$_` to the candidate value being checked — a sibling reference
like `$x` inside `$y`'s `where` clause resolved to whatever `$x` happened
to be in the enclosing scope (usually undefined), not the actual candidate
value. Real call binding never had this gap (`binding_signature.rs` binds
parameters one at a time into the same `self.env`, so a later parameter's
`where` clause already sees earlier siblings) — only the separate
ACCEPTS/smart-match path was missing it.

Fixed by pre-binding every parameter's resolved value into `self.env`
under its bare name before running any `where`-clause checks, then
unwinding those bindings afterward regardless of the match outcome
(mirroring what real binding already does, scoped to the ACCEPTS call so
nothing leaks into the caller).

## Effect

`t/http-router.rakutest` (vendored Cro::HTTP suite): the router's
`request-body "application/json" => -> (:$x, :$y where $y > $x) {...},
-> (:$x, :$y where $y <= $x) {...};` Pair-signature dispatch now correctly
picks the first block when its `where` clause's sibling-referencing
condition holds, instead of falling through to the second/fallback block.
Combined with the two other general binding bugs fixed earlier this week
(`signature-accepts-slurpy-where-constraint.md`,
`named-param-type-constraint-enforced-at-binding.md`), `t/http-router.rakutest`
now passes all 360 of its counted subtests (an unrelated, separately-tracked
multipart/form-data body-parsing issue remains after that point — see
`todo/tickets/multipart-form-data-body-not-destructured-in-request-body-handler.md`).

Pin: `t/signature-accepts-sibling-named-where.t`.
