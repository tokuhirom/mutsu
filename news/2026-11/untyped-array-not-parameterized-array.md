# An untyped Array or List no longer satisfies `Array[Int]`

`(1,2,3) ~~ Array[Int]`, `[1,2] ~~ Array[Int]` and the equivalent parameter binding were True
because a parameterized `Array`/`List`/`Positional` constraint fell back to checking the elements of a
container with no declared element type. rakudo answers False: `Array[Int]` is a distinct type, so
only a container declared with the element type matches. A subset over `Array[Int]` now coerces a
plain list into a real `Array[Int]` (#15336). The local `t/regex/rx-delimiters-and-smiley-param.t`
passed untyped arrays to `Array:D[Int]` parameters and now passes `Array[Int].new(...)`.
