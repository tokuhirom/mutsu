# A Unicode infix alias resolves to the same routine by name

`say 4 ≅ 4` worked; `say &infix:<≅>(4, 4)` died with `Two terms in a row`. So
did `⩵`, `⩶`, `≠`, `≤` and `≥`. The parser recognises each Unicode spelling
inline and maps it straight to the operator's `ComparisonOp`, but nothing did
that mapping when an operator was reached by *name*: the four sites that
dispatch an `infix:<op>` call each carried the same one-off
`if op == "−" { "-" }` normalisation and no more, so `≅` reached
`build_infix_expr` as itself, became a `TokenKind::Ident("≅")` binary that no
compiler arm claims, and blew up as a syntax error at runtime.

`Interpreter::normalize_unicode_infix` now holds the whole alias table in one
place — the ten spellings the parser accepts — and the four dispatch sites plus
`infix_token` go through it. rakudo agrees on the identity: `&infix:<≅>.name` is
`infix:<=~=>`.

The native `cmp-ok` had the same hole from the other direction: its string
operator table listed `=~=`/`≅` but not `≤`, `≠`, `⩵` or `⩶`, and answered
`cmp-ok: unsupported string operator '≤'`. It goes through the same table now.

Found under the real `Test` module: `cmp-ok`'s way of turning a string operator
into a callable is `&CALLER::LEXICAL::("infix:<$op>")`, so
`roast/S32-num/complex.t`'s `cmp-ok 42, '≅', 42+0i` aborted the file's
`Real ≅ Complex` subtest — and with it the rest of the file — before its first
assertion. `complex.t` now passes under `MUTSU_REAL_TEST=1`.

Pin: `t/unicode-infix-alias-by-name.t` (all fourteen assertions verified against
`raku`).
