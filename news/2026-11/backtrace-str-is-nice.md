# `Backtrace.Str`, `print $bt`, `put $bt` and `"$bt"` are `.nice`

A `Backtrace` stringified by any route other than the explicit `.nice` printed every
frame, internal `throw`/`die` frames included, with doubled indentation, and
`.Str` had no trailing newline. All of them now give `.nice`, the interesting
frames one per line, newline-terminated, as rakudo does (#16848; found by the
raku-doc sweep, `Type/Exception.rakudoc`).
