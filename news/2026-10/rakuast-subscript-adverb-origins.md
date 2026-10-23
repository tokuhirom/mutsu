# RakuAST subscript adverbs and interpolated subscripts carry their origins

`.AST` now places the adverbs written after a subscript, as Rakudo does: the
subscript's range runs over them (`@x[0] :v`, `%h{"a"}:exists`,
`%h<a> :!delete`), and each `ColonPair::True` / `ColonPair::False` has its
own. They are read back from the end of the subscript's text; an adverb with
a value (`:k(1)`) is not, and leaves the subscript without a range. Before,
only a single adverb written against a literal hash subscript was placed, and
any other adverb left the whole subscript and its index without one.

An interpolated subscript or method call in a string (`"a @x[0] b"`,
`"a @x.elems() b"`) is now placed too: the application, its variable, its
subscript or `Call::Method`, and the literal text after it, which used to
stop the placing of the string's segments. A subscript or argument list in
the string holding a quote, a backslash or a comment, which could hide its
closing bracket, is left alone. A string made of a single interpolated block
(`"{ ... }"`) is placed as well.
