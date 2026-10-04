# `{ ... }.lazy` dies "No such method" instead of running the block

The `lazy` statement prefix parsed `lazy { ... }` into the same AST as the
method call `{ ... }.lazy`, and the compiler lowered both to `(do { ... }).lazy`
so the prefix would run its block. That made the method call run the block too:
`{ $_ }.lazy()` reported a missing `lazy` on `Any` (the block's result) rather
than on `Block`, and `(-> {}).lazy` was silent.

The prefix now re-hosts its block as `do BLOCK` in the parser, where the two
forms are still distinguishable, and the compiler's special case is gone. A
`.lazy` on any Code object now dies `X::Method::NotFound` naming `Block` or
`Sub`, as rakudo does. Closes #11629; the did-you-mean wording is #11816.
