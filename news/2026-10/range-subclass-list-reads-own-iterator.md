# `.list`, `.List`, `.Array` and `.join` of an `is Range` subclass read its own `iterator`

An `is Range` / `is Seq` subclass that declares `iterator` listed the range or
seq it carries and ignored the override, although Rakudo's `Range.list` is
`self.iterator`-based. The `self.list` methods now go through the override
(`.elems` stays the range arithmetic, as in Rakudo). Found with
`Math::Sequences` (#15361).
