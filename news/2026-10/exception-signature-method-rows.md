# `Exception`, `X::AdHoc`, `CX::Warn` and `Signature.gist` are rows

`message`, `gist`, `Str`, `backtrace` and `resume` of `Exception`, `payload` and
`message` of `X::AdHoc`, `message` of `CX::Warn` and
`X::TypeCheck::Assignment`, and `Signature`'s `gist` and `raku` are rows of the
method table now (ADR-11276 §9.39). The ~190-line exception block of the
zero-argument cascade, whose gist/Str/message arms repeated the same
"no message" fallbacks, is one call into the rows' handlers.
