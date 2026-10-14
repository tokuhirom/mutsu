# RakuAST round-trips an immediately called sub

`sub foo($n) { $n * 2 }(21)` and `my sub bar { fail }()` now convert to
rakudo's `ApplyPostfix(operand => Sub, postfix => Call::Term)` and lower back
to the parser's own declaration-then-call expansion, so the routine is still
declared where it stands. The parser marks the expansion with a source-form
record instead of the converter guessing from its shape. Four files that were
refused under `MUTSU_RAKUAST=1` (the immediate-call and backtrace-frame tests)
now pass; `t/rakuast/rakuast-immediate-sub-call.t` pins the node and the
round trip.
