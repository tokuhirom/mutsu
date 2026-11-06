# A built-in iterator's `is-lazy` on a call result

`(1..*).iterator.is-lazy` and `[4,5,6].lazy.iterator.is-lazy` answered `False` when the program
composed the `Iterator` role (the role's default `is-lazy` won on a non-variable receiver).
The `CallMethod` path now tries the built-in iterator rows first, as the variable path already
did, so both answer `True` like rakudo (#16934).
