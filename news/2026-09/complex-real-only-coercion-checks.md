# `.Int` on a Complex silently dropped the imaginary part

`(1+2i).Int` returned `1` instead of throwing `X::Numeric::Real` — the coercion
matched on `ValueView::Complex(r, _)`, ignoring the imaginary part entirely.
`.Num` already had the check; `.Real` had it too but rendered the message as
the literal word `Complex` instead of the value (`Cannot convert Complex to
Real: ...` instead of `Cannot convert 1+2i to Real: ...`); `.Rat` and `.FatRat`
threw a bare `X::AdHoc` instead of a typed `X::Numeric::Real`, with the same
`Complex`-literal message bug.

All five coercions (`.Int`, `.UInt`, `.Num`, `.Rat`, `.FatRat`, `.Real`) now
share one exception builder (`complex_not_real_exception`/`complex_not_real_error`
in `dispatch_core_coerce.rs`) that renders the source value and reports the
actual attempted type via `.target` (`Int` for `.Int`, `UInt` for `.UInt`, ...),
matching raku's own message: `Cannot convert 1+2i to Int: imaginary part not
zero`. A purely real Complex (zero imaginary part, any sign) still coerces
normally through every one of them. (#9814)
