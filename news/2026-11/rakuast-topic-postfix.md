# RakuAST: `.++` and `.--` are topic calls

`.++` and `.--` increment or decrement the topic. The parser runs them as
`$_++` / `$_--`, so RakuAST viewed them as an `ApplyPostfix` over `$_` and
deparsed `.++` as `$_++`. Rakudo's RakuAST frontend keeps them as a
`Term::TopicCall` whose call is the `Postfix` itself.

The parser now marks the postfix application with
`Spelling::TopicPostfix`. The view renders the topic call (in the new
`src/rakuast/topic_postfix.rs`), and lowering maps a
`Term::TopicCall(Postfix)` back to the postfix application on `$_`. Found
through the `.DEPARSE` corpus (`S03-operators/autoincrement.t`, #14880).
