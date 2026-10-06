# Missing package-stash keys use `Any`

Reading an absent key from a package `Stash`, including `GLOBAL::` and
`PROCESS::`, now returns the `Any` type object as an ordinary hash does.
Lexical `PseudoStash` misses continue to return `Nil`.
