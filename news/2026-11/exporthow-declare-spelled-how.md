# `EXPORTHOW::DECLARE` keywords whose HOW is an imported type

The module scan read `constant kw = SomeHOW` in `EXPORTHOW::DECLARE` only when the right-hand side
was a plain bareword. Once the exporting module imported the HOW (here through a module with a
`sub EXPORT` hook, Qwiratry's `Transformer` HOW), the parser wrapped it as a spelled term, the
keyword was silently dropped, and `transformer Foo is Transformer { }` was "Two terms in a row".
The scan now looks through the wrapper.
