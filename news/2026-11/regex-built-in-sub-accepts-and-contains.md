# A regex built in a sub keeps its interpolated value in `Regex.ACCEPTS` and `Str.contains`

`my sub tm(Str $g) { /$g/ }` returns a regex that closes over the sub's `$g`. `~~` and `.match` read
it through the scope the regex closed over, but `Regex.ACCEPTS` and `Str.contains(Regex)` matched
without that scope, so `tm("abc").ACCEPTS("xabcx")` was `Nil` and `"xabcx".contains(tm("abc"))` was
`False`. Both now go through the same scope installation `.match` uses. Closes #16691.
