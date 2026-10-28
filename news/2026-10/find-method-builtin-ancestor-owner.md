# `.^find_method` on a subclass of a built-in type names the built-in owner

`class H is Hash does Wrap` where `Wrap`'s body captures
`::?CLASS.^find_method('AT-KEY')` got a Method object owned by `H`; calling it
on an `H` looked the name up on `H` again and found the role's own wrapper,
recursing forever (WriteOnceHash). The object is now owned by the built-in
ancestor that declares the method (`Hash`), as in rakudo. WriteOnceHash's
`t/01-basic.rakutest` now reaches test 19 (from 2); the `my %h does Role = ...`
declaration initializer still lacks the `:INITIALIZE` flag.
