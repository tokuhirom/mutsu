# RakuAST round-trips `self!"$name"()`

A private call with a quoted name (`self!"$n"()`, `self!"literal"()`) was refused by the RakuAST converter. It now renders rakudo's `Call::QuotedMethod` with `dispatch => "!"` (a bare `!`, unlike the `.?`/`.+`/`.*` spellings), and lowering restores the private modifier. `t/oo/mop/metamodel-private-methods.t` and, together with the dispatch-modifier change, `t/oo/method/dynamic-method-name-matches-static.t` join the round-trip ratchet.
