# RakuAST: `is export`, `is rw`, `is raw` and `our` subs cross the boundary

`.AST` refused every sub declared `is export`, `is rw` or `is raw` as a "sub
with traits / multi / export". It was the second most frequent refusal under
`MUTSU_RAKUAST=1`, and any module that exports a sub hit it.

Measured on rakudo 2026.09, each trait is a `Trait::Is` in the sub's `traits`,
placed before its `body`. A bare `is export` has no argument. `is export(:a)`
carries `argument => Circumfix::Parentheses(…ColonPair::True("a")…)`, and
several tags carry an `ApplyListInfix(",")` of colonpairs.
`rakuast::routine_traits`, which already handled methods, now renders and
lowers subs too. It reads export tags back into `SubDecl.export_tags`; a
method's `is export` takes the same path where the converter used to drop it.

An `our sub` (the parser's `__our_scoped` marker) is `scope => "our"` ahead
of everything else; a `my sub` renders no scope, as in rakudo.

The parser records a bare `is export` as the `DEFAULT` tag, so `is export(:DEFAULT)`
comes back bare. The two mean the same thing. A routine carrying more than one
of these flag traits, or one of them together with `returns`/`of`, stays
refused, because the parser does not keep their source order.

Writing the round-trip test turned up an unrelated bug, filed as #10965:
assigning to a call of a named `is rw` sub returned from `EVAL` resolves the
sub by its declared name.

The round-trip ratchet grows from 1580 to 1599 of 5808 `t/` files. Pinned
by `t/rakuast/rakuast-sub-traits.t`, which passes under both mutsu and raku.
