# `Str.comb(Regex)` honours `<(` and `)>`

`"abcd".comb(/a<(b)>/)` now returns `("b",)` like Rakudo, instead of the whole match `"ab"`.
`.comb` returns each match's `.Str`, which the capture markers narrow; a regex containing a marker
now takes the capturing search path and uses the narrowed span.
