# RakuAST: subscript adverbs cross the boundary

`.AST` refused the subscript adverbs `@a[0]:exists`, `%h{"x"}:delete`,
`@a[1]:kv` and their combinations. The parser builds them straight into the
shapes the compiler executes: an `Expr::Exists` node, a `DELETE-KEY` call or a
`__mutsu_subscript_adverb` builtin call. The converter met only those shapes.
A survey of the `t/` files outside the round-trip ratchet found these adverbs
behind the first refusal of 117 files.

Measured on rakudo 2026.09, a subscript's adverbs are the postcircumfix's own
`colonpairs`, kept in source order:
`Postcircumfix::ArrayIndex(index => …, colonpairs => (ColonPair::True("exists"),))`.
The builders of the executed shapes now live in `ast::subscript_adverb`, and
the parser calls them as it reads each adverb. `ast::subscript_adverb::expand`
composes them for a whole colonpair list, and `rakuast::lower` calls it. The
converter reads an expansion back with `ast::subscript_adverb::adverbs`. It
accepts the reading only when `expand` rebuilds exactly the same expression,
so no shape is reverse-engineered by guesswork. The three `Postcircumfix::*Index`
classes also gained the `colonpairs` field and a `.new` constructor.

Two parser inconsistencies surfaced along the way:

- `%h<a>:delete(0):exists` deleted the key. It ignored the `:delete` argument
  that the `:exists:delete(0)` order honoured. It now deletes only when the
  condition holds, as in rakudo.
- `:delete:k` became a `DELETE-KEY(k => True)` call, while `:k:delete` became
  the adverb builtin with a delete flag. Both orders now build the second
  shape.

Multi-dimensional subscripts (`@a[0;1]:exists`) and the zen slice `@a[]:delete`
still refuse. An array's `:v:delete` storing `Any` instead of a hole is #11320.

The round-trip ratchet grows from 2438 to 2502 of 6004 `t/` files. Pinned by
`t/rakuast/rakuast-subscript-adverbs.t` and
`t/collections/subscript/subscript-delete-cond-exists.t`. Both pass under mutsu
and raku.
