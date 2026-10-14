# `Nil.EXISTS-POS` and `Nil.EXISTS-KEY` are `False`

`Nil` absorbs a method call it does not define, answering `Nil`, and mutsu's interpreter did that for `EXISTS-POS` and
`EXISTS-KEY` too, before any dispatch. Rakudo's `Nil` reaches `Any:U`'s candidates for both, which answer `False`, so the two
names now fall through the `Nil` short-circuit (the plain, bound-name and hyper forms) to the dispatch that already said
`False` (#13294).
