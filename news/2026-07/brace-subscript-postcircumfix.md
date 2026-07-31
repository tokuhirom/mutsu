# No-whitespace postcircumfix `{ }` binds on call results and parenthesized expressions

Fixed in #5599 (2026-07-31, found by the web-framework battery survey,
`docs/batteries/web-framework.md`). raku binds a `{ ... }` that immediately
follows a term — no whitespace — as a hash subscript, but three mutsu parse
paths dropped it:

- the listop call path consumed `routes{'/'}` as a hash-composer *argument*
  (imported subs like Humming-Bird's route table died with "Too many
  positionals passed; expected 0 arguments");
- the `Type{...}` constructor shorthand swallowed the same shape for
  locally-declared subs — known routines now fall through to the subscript
  arm;
- the postfix hash-index arm rejected parenthesized binary/ternary targets
  because `paren_expr` returns them unwrapped, so
  `($a // $b){$key}.List` (the exact `Cro::HTTP::Router` line-188 shape)
  escalated to a parse error in statement position.

Pin: `t/brace-subscript-postfix.t` (9/9 under raku too). Effect: the
Humming-Bird upstream suite went 5/14 → 9/14 and `use Cro::HTTP::Router`
parses; the next Cro blocker (Log::Timeline eagerly loads `CBOR::Simple`,
which needs the nqp buffer-op family) is filed as
`todo/tickets/cbor-simple-nqp-buf-ops.md`.
