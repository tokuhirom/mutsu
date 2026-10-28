# `:exists` runs the Bool of an object EXISTS-KEY answers

A user `EXISTS-KEY` that answers an object (Red's AST node, whose
`Bool` throws a control exception to learn what a `grep` block tests) had the
answer tested with the raw truthiness, so its own `Bool` never ran and
`.json-column<bla>:exists` inside a Red `grep` always meant "false" (Red
t/87-json-operations). The subscript `:exists` adverb now coerces the answer
through `Bool` (`eval_truthy_result`), as rakudo does, and lets an exception it
throws unwind the statement.
