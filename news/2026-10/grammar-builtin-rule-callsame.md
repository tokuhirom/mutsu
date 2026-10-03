# A grammar's own `method ws` can `callsame` into the built-in rule

A grammar (or a role it does) that overrides a built-in rule as a method —
`ws`, `wb`, `ww`, `ident`, `alpha`, `digit` and the other character-class
rules — can now defer to the built-in with `callsame` / `nextsame`. In Rakudo
those rules are ordinary methods of `Match`, so the built-in is simply the next
candidate in the MRO; mutsu matches them inline and had no candidate to defer
to, so the deferral answered `Nil` and every `:sigspace` rule of such a grammar
failed to match.

The built-in rule is now the final candidate of the deferral chain: it runs at
the invocant cursor's position and answers the advanced cursor (or a failed
one, `pos == -3`). The `wb` / `ww` / `ws` subrule checks the engine already had
share the same implementation (`regex_builtin_rule::builtin_rule_end`).

This is the "good parse errors" idiom from Moritz Lenz's grammar book, used
verbatim by `DSL::Shared::Roles::ErrorHandling`; with it,
`DSL::Entity::Foods`' `t/Food-names-parsing.t` passes (4/4, was 0/4). The
high-water-mark `$*HIGHWATER` write that idiom makes still does not reach the
`parse` frame — that is #11326.
