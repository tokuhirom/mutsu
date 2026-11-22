# `.Capture` of a type object answers as Rakudo's

Rakudo's `Mu.Capture` reads the public attributes of `self`. On a type object
of a class with none (`Mu`, `Any`, `Cool`, `Exception`, `Nil`) that is
`\()`. On one with attributes (`Rat`, `Pair`, `Date`, `X::AdHoc`, a user
`class Q { has @.a }`) the read dies:
"Cannot look up attributes in a Q type object. Did you forget a '.new'?". A
class whose own `Capture` is `:D` (`List`, `Map`, `Range`, `Match`) refuses
the type object as its invocant. mutsu put the type object in a one-positional
`Capture` (`\(Mu)`), absorbed `Nil.Capture`, or reported no such method (#18778).

What each core class's type object does is now a table,
`src/builtins/methods_0arg/builtin_type_capture.tsv`, generated from Rakudo by
`scripts/builtin-type-capture.raku` (1029 classes). A user class walks its
MRO: the first class with a public attribute, or the first core class the table
lists, decides. `Nil.Capture` reaches `Mu.Capture` instead of being absorbed.
