# Literal sub-signature parameters select candidates; junction constructors splice Slips

- `multi ec(:@op ('in', $l, *@values))` matched any first element during candidate selection, so
  SQL::Abstract rendered `<` as `IN`. A literal parameter in a sub-signature now has to match (#16777).
- `none(1..42, Empty)` kept `Empty` as an eigenstate; a Slip argument is spliced into the junction's
  arguments as in rakudo, giving SQL::Abstract's `NOT BETWEEN` (#16777). SQL::Abstract's own
  `t/01-basic.rakutest` now passes 140/140, matching rakudo.
