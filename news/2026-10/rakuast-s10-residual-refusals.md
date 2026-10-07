# RakuAST S10: the first batch of residual refusals

Stage 1 slice S10 of the RakuAST frontend campaign (#7564) started from a fresh
`scripts/rakuast-frontend.sh causes` survey after S9: 886 of 6766 `t/` files
were outside the round-trip ratchet (707 refused, 160 ran differently). This
batch moves 95 of them in, so `ci/rakuast-frontend-passing.txt` lists 5975
files (was 5880).

What now crosses the boundary:

- `nqp::const::NAME` as `Nqp::Const` and an `nqp::op` written without
  parentheses as `Nqp` (the injected call-site line argument no longer leaks
  into a parenthesised one).
- A CORE term keyword that the parser keeps shadowable after a run-time
  `sub EXPORT` import (`True`/`False`, #9047) round-trips and stays shadowable.
- `do whenever ...`, and `when` / `default` in expression position.
- `proceed`, a bare `succeed` and `take-rw` as bare calls, and `last` / `next`
  / `redo` in expression position (`COND or next`) as `ControlFlow`.
- `PRE` / `POST` phasers in rakudo's shapes (`Phaser::Pre` over a called block,
  `Phaser::Post` over the block, the bare statement forms). The verbatim
  condition text that `X::Phaser::PrePost` quotes rides in a hidden
  `condition-source` field, like a statement's `origin`.
- `STMT when COND` as a `StatementModifier::When`.

What the survey still lists is a long tail of single causes (see the issue
comment for the numbers); the largest clusters left are the parser's
`with`/`orwith` pointy-signature desugaring, the `:(...)` signature literal
(the parser builds a runtime `Signature` value and keeps no source), word lists
(#12199) and the regex declarations without a source tree.
