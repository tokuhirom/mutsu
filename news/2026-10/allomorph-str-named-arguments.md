# An allomorph's .Str ignores :superscript and :subscript

`<5>.Str(:superscript)` answered `⁵`: `Str` was on the allomorph's own-name
list, so the named-argument call reached the `Int` part. Rakudo's
`Allomorph.Str` is the string part whatever named arguments are passed. `Str`
now leaves that list and the allomorph is answered by the `Str` row on its
string part (#13259).
