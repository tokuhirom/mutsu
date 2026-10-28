# An object hash keeps its entries when it is handed to a `%` parameter

`my %h{List}; %h{$array} = 1` filed the entry under a string built from the
itemized key's contents, because the subscript paths wrap an itemized key in a
`Scalar` and `value_which_key` did not look through the wrapper. Binding the
hash to a `%` parameter re-tags it, and the tag step re-keys every entry by its
object's `.WHICH` (`Array|<id>`), so lookups, `:exists`, `:delete` and stores
made with the wrapped key then missed (a second entry appeared, and the
caller's own lookups failed too). `value_which_key` now sees through the
`Scalar`, so every path files an itemized key under the same string.

This is what made CSS::Stylesheet's `!media-slot` emit the outer `@media`
block several times.
