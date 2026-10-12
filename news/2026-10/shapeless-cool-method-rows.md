# Built-in methods reach receivers with no shape through the method table

The built-in method table answers a call by the receiver's shape (`Str`,
`Int`, `List`, ...). A receiver with no shape of its own reached `Cool`'s text
methods only through name-matching cascade arms, each of which turned the
receiver into a string on its own. That kept 34 of those arms alive even
though every method already had a row in the table.

The table's guard step now resolves such a receiver itself. An instance of a
user class deriving from a built-in scalar type (`class MyStr is Str`,
`class MyInt is Int`) is answered by the rows of the value it wraps. An enum
value, a `Seq` that is still being produced and an instance of a class
`is Cool` reach `Cool`'s rows. `Range`, `Seq` and `IO::Path` reach an audited
list of `Cool`'s text rows. The arms for `trim`, `flip`, `chomp`, `chop`,
`comb`, `words`, `lines`, `codes`, `fmt`, the case maps, `encode`, the
normalization forms and the per-codepoint Unicode methods are gone.

Several answers now match Rakudo. `MyStr.new(value => "x").chomp`,
`.naive-word-wrapper` and `.Date` work where they used to throw
`X::Method::NotFound`, and `"x".IO.chomp` reaches `Cool.chomp`. A value that is
not a `Cool` (`Pair`, `Set`, `Capture`, `Sub`) no longer answers `.flip` or
`.trim` with text built from its string form, and raises
`X::Method::NotFound` as Rakudo does.
