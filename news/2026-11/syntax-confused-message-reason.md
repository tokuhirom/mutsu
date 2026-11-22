# `X::Syntax::Confused.message` is its reason

In rakudo, `X::Syntax::Confused` reports its reason as the message: `say "a"
rescue "x"` fails to compile with `Two terms in a row`. mutsu spells the
diagnosis "Confused. {reason}" internally and leaked that spelling into both the
exception's `.message` and the printed compile error.

`.message` is now the reason, and so is the `===SORRY!===` body. Only the raw
parser-expectation fallback, which has no rakudo reason, keeps the "Confused"
marker (rakudo prints plain `Confused` there).
