# Empty typed array pop/shift names `Array[T]`

`pop` and `shift` on an empty `my Int @t` now report `Cannot pop from an empty Array[Int]`
(previously `Array`), matching Rakudo (#12242).
