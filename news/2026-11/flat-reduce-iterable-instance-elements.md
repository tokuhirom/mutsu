# `flat` and `[op]` read Iterable instances

`(C.new, 9).flat` (also `flat(C.new, 9)`) now reads the `iterator` of an
`Iterable` element, as `for`, `.map` and `my @a =` already did, and so do
instances of `is Range` / `is Seq` subclasses: `(Q.new(1, 3), 9).flat` is
`(1 2 3 9)`. A reduction over a bare Range/Seq-subclass instance folds its
elements (`[+] Q.new(1, 3)` is 6); a `$` variable holding one stays a single
item (`[+] $q` is 3) and a `$`-held Seq is still iterated, as in rakudo. An
`Array` element stays an item, a user `does Iterable` class stays one operand
of a reduction.
