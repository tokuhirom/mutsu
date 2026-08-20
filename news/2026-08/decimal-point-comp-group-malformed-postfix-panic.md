# `5.`'s `X::Comp::Group` now carries rakudo's real `X::Syntax::Malformed` panic

`EVAL '5.'` throws an `X::Comp::Group` in both mutsu and rakudo, and both
agreed on `.sorrows[0]` being `X::Syntax::Number::IllegalDecimal` — but the
group's `.panic` used to differ: rakudo reports `X::Syntax::Malformed`
("Malformed postfix call", because after rejecting `5.` as a number it
retries the trailing `.` as a method-call postfix, finds no method name, and
panics with `what => 'postfix call'`), while mutsu reported a placeholder
`X::Comp::AdHoc` with message "Confused". mutsu's combined `.message` was
also missing rakudo's second line ("Malformed postfix call").

`illegal_decimal_point_error()` in `src/parser/expr/postfix/loop_.rs` built
the error group with a placeholder panic via `PError::comp_group(sorrow,
false, "Confused", MSG.to_string())`, which always wraps whatever string it
is handed in a generic `X::Comp::AdHoc`. Switched it to
`PError::comp_group_with_panic` with a real `X::Syntax::Malformed` exception
(`what => "postfix call"`, `message => "Malformed postfix call"`), matching
the attribute shape `PError::malformed()` already builds for the same
exception class. The group's own combined `.message` is now the sorrow's
message and the panic's message joined by a newline, exactly matching
rakudo's `"Decimal point must be followed by digit\nMalformed postfix
call"`.

`t/decimal-point-illegal-comp-group.t` was extended to assert `.panic ~~
X::Syntax::Malformed`, `.panic.what eq 'postfix call'`, and the full two-line
`.message`, in addition to the existing `.^name`/`.sorrows[0]` checks.
`roast/S32-exceptions/misc.t` (already whitelisted) stays green — its
assertion only inspects `.sorrows[0]`, so it was unaffected by the panic
shape either way.

This was a straightforward implementation of
`todo/tickets/illegal-decimal-comp-group-panic-shape.md`, split out earlier
from a stale "comp-group multi-error bundling" investigation. The
`when SomeUndeclaredType { ... }` residual mentioned there (a bareword
gobbling the following block) has a different root cause and stays tracked
separately in
`todo/deep/when-undeclared-bareword-gobbles-block-needs-cross-file-type-index.md`.
