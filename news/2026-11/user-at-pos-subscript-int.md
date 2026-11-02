# A user `ASSIGN-POS`, `BIND-POS` and `EXISTS-POS` get the subscript as an `Int`

`@o[2.9] = v`, `@o[0.6] := v` and `@o[1.2]:exists` on an object with its own `ASSIGN-POS`,
`BIND-POS` or `EXISTS-POS` handed the method the index as written (`2.9`), where Rakudo's
`postcircumfix:<[ ]>` calls it with the `Int` (`2`); the read form (`AT-POS`) already did. A `[ ]` index
is now made an `Int` once at the store and `:exists` ops. The key-returning forms (`:exists:kv`) keep
the index as written. A base `Array.BIND-POS` reached through `nextwith` therefore sees an `Int`, so
Array::Rounded-style overrides that round their own index no longer hit "Cannot BIND-POS with a
negative index" (#15996).
