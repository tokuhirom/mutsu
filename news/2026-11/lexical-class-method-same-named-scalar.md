# A lexical class's method tells `$R` from the class `R`

`sub f { my $R = 5; my class R { method r { $R } }; R.new.r }` returned the
type object `(R)` instead of `5`. mutsu keeps a scalar under its sigil-less env
key, which is also the key a type name is read by. A method body compiled both
`$R` and a bare `R` to the same by-name read, and its declaration-time capture
bound the class over the scalar's cell.

A method body now inherits the lexical types its enclosing blocks declare,
its own class included, and reads them by their type-only key, as a closure
body already did (#17711). The capture accepts that key. When the class is
block-declared, it leaves the bare key to the declaring frame's same-named
scalar, so the method reads and writes that variable (#18327).
