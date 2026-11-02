# `:exists` reaches a user `postcircumfix:<[ ]>` for an `is Array` subclass

`@a[1.5]:exists` on an `is Array` subclass whose module declares
`multi postcircumfix:<[ ]>(Rounded:D \SELF, Any:D \index, |c)` answered from the core
routine and ignored the user candidate (`False` where rakudo says `True`, since the
candidate rounds the index). A positional `:exists`/`:delete` now considers a user
candidate whose invocant is typed with a user class the target is an instance of; a plain
`Array` keeps the core routine. Found with Array::Rounded's `t/01-basic.rakutest`.
