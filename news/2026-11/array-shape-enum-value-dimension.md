# An enum value as an array shape dimension is its numeric value

`my @a[Deuce;Deuce]` took the *size of the enum* for each `Deuce` (`(4 4)`),
because the shape reader treated a `Cards` enum value like the `Cards` type
object. Only the type object counts variants; a value is its integer
(`(2 2)`), and a zero or negative value is an illegal dimension. Found by the
raku-doc differential (`Language/list.rakudoc`).
