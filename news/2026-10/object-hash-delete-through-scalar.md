# `$h{4}:delete` on an object hash held in a `$`

A `Hash[UInt, Any]` (an object hash: its entries are keyed by the key object, not its
string form) held in a scalar variable ignored `:delete` by key, because the named
delete op only encoded the key when the *variable* carried a key constraint and a `$`
has none of its own. The key is now encoded from the held hash, and the expression
form (`holder(){4}:delete`) does the same. It is one of the two causes of
App::RaCoCo's `DataPart.plus` giving `4 1 9 1` instead of `9 1`.
