# Any's subscript protocol answers type objects and user objects

`AT-POS`, `EXISTS-POS`, `AT-KEY` and `EXISTS-KEY` on a value that is neither `Positional` nor
`Associative` are now rows of the built-in method table, as Rakudo's `Any` candidates are. A value
is the one-element list holding itself (`5.AT-POS(0)` is `5`, an instance of a user class answers
itself at index 0), a type object answers itself at 0 but holds nothing (`Int.EXISTS-POS(0)` is
`False`) and autovivifies to `Any` under a key (`Int.AT-KEY("x")`), and a defined value fails the
associative subscript. Type objects and user instances used to get `X::Method::NotFound` for the
positional pair. A `Match` indexes its positional captures through `Capture`'s rows, and an optional
group that did not match does not exist.

See ADR-11276 §9.106.
