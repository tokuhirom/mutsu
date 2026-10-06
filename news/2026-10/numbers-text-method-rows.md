# Numbers and text: 284 more built-in methods are table rows, and Bool inherits Int's

Slice 3B of [ADR-11276](../../docs/adr/11276-built-in-methods-are-handler-rows.md) moved the
transcendental math methods (`sin`, `cos`, `tan` and their reciprocal, hyperbolic, inverse and
inverse-hyperbolic forms, `exp`, `log`, `log2`, `log10`, `sqrt`, `atan2`, `cis`, `unpolar`, `polar`,
`roots`, `expmod`), `is-prime`, `narrow`, `conj`, `Bridge`, `lsb`, `msb`, `chr`, `rand`, the native
integer coercions, `Cool`'s `abs`/`sign`/`floor`/`ceiling`/`truncate`/`round` (and `round($scale)`),
the Unicode methods (`uniname`, `uniprop`, `unival`, `unimatch`, `uniparse`, `NFC`, ...) and `Uni`'s
own methods into the built-in method table: 676 rows are registered now, up from 392. A `Cool` row
reads its receiver through one shared numification, so `"0.5".sin`, `[1, 2, 3].sin` and `0.5.sin`
are one implementation.

The `Bool` shape is open to its ancestors' rows, after an audit of every registered `Int`, `Cool`
and `Any` row against `True` and `False`: `True.sin`, `True.round(1)` and `True.atan2(2)` work, and
`True.abs`/`True.floor` answer as Rakudo does.

Answers that moved toward Rakudo: `"abc".sin` is the `X::Str::Numeric` failure, `[1,2,3].is-prime`
numifies its List, `"5".lsb` and `5.polar` are "No such method" (Rakudo declares them on `Int`
and `Complex` only), `65.5.chr` is `A`, `Uni.AT-POS` works, `Str.unival` raises `X::Multi::NoMatch`.

`RowFlags::RANDOM` marks a row whose answer is random (`rand`): the debug cross-checks skip it.
