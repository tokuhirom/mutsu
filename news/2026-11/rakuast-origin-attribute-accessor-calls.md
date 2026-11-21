# `.origin` of public attribute accessor calls

A call through a public attribute's accessor sigil now carries the ranges
rakudo 2026.11 gives it:
- **Accessor calls with arguments** (`$.foo(1)`, `@.lst(3, 4)`, `$.foo: 5`):
  the `ArgList` covers the arguments.
- **`$.^name`**: rakudo builds it as `(self.^name).item`. Its
  `Call::MetaMethod` covers the name after `$.^`, with an empty `ArgList` at
  the end. The application inside it stays without a range, as on rakudo.

(#15780)
