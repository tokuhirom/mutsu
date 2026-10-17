# Complex.sign, Int.pick and Int.roll follow language revision 6.e

Under `use v6.e.PREVIEW`, `(1+2i).sign` is now the unit complex number and
`5.pick` / `5.roll(3)` sample `^5`, as in Rakudo's `core.e` setting. Under
6.d, `Complex.sign` and `Int.pick`/`roll` keep their 6.d answers: the first
is `Cool.sign`, which refuses a non-real `Complex`, and the other two are
`Any.pick`/`Any.roll` of the one-item list. The method table looks up such a
6.e row ahead of the receiver shape's entry when the program's revision is
6.e (ADR-11276 §9.149).
