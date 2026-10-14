# RakuAST keeps the dispatch modifier of a quoted method call

`$x.?"name"()`, `.+"name"()` and `.*"name"()` were refused by the RakuAST converter ("quoted method name with a modifier"). They now render rakudo's `Call::QuotedMethod` with a `dispatch => ".?"` field (literal and interpolated names alike), and lowering restores the modifier, including on the hyper form. `t/oo/method/topic-quoted-method-name-modifier.t` joins the round-trip ratchet.
