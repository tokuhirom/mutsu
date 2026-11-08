# `.AST` of a fat arrow with a quoted key

`("a" => 2)` came out of `.DEPARSE` as `(a => 2)` and `("a b" => 2)` as `(a b => 2)`, which is not
valid Raku. Rakudo's tree for a quoted key is an `ApplyInfix` over a `QuotedString`, where a bare
`a => 2` is a `FatArrow`; the parser now records the quoted key (`BinaryForm::QuotedKey`) and the
view renders it that way (#14845). A quoted key inside a `{...}` / `%(...)` hash literal still
prints bare (the hash literal stores its keys as plain strings).
