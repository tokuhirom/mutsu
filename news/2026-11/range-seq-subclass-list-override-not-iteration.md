# A Range/Seq subclass's own `list` no longer drives `@`-assignment or `for`

`class S is Range { method list { (7, 8).list } }; my @d = S.new(0, 3)` stored `[7 8]`; rakudo
iterates `self.iterator` (inherited from `Range`) and answers `[0 1 2 3]`, with the subclass's `list`
used only by an explicit `.list` call. `for S.new(0, 3)` had the same inversion. The payload
iteration now reads the base type's own `list` on the carried range/sequence and still defers to a
subclass's own `iterator` (#16013). `.map`, `.elems` and `.Array` on such a subclass still differ;
see #16022.
