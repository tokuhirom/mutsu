# A Str that numifies to a Complex coerces as that Complex

Rakudo's `Str.Int`, `.UInt`, `.Num`, `.Rat`, `.FatRat` and `.Real` are
`self.Numeric.METHOD`, so `"1+2i".Int` is `Complex.Int`: the imaginary part is
tested against `$*TOLERANCE`, a negligible one leaves the real part to coerce,
and any other is `X::Numeric::Real` (a lazy `Failure` for `.Real`).

mutsu numified the string and then truncated the result without looking at its
imaginary part: `"1+2i".Int` was `1`, `"1+2i".Num` was `0`, and
`"3+1e-20i".Num` was `0` where Rakudo answers `3`
([#11984](https://github.com/tokuhirom/mutsu/issues/11984)).

The builtin `Str` arms of the six methods now ask one shared predicate,
`str_numifies_to_complex` (`src/value/str_numeric.rs`, O(1) for any string that
does not end in `i`), and decline such a string. The runtime step that already
answers a `Complex` receiver through `Interpreter::dispatch_complex_to_real`
(#11795) then coerces the parsed number the same way, so a `Str` and a
`Complex` can no longer disagree, and an error names the number the string
spells (`Cannot convert 1+2i to Int: imaginary part not zero`).

Pinned by `t/types/string/str-complex-string-coercion.t`, whose expectations
were taken from `raku`.
