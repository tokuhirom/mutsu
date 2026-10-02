# A strict force of an infinite sequence throws instead of truncating

A sequence-spec lazy list is always infinite. That covers an unbounded Range
stepped by `.succ` (`1..*`, `"a"..*`), an arithmetic or geometric `...` sequence
(`1, 3 ... *`, `1, 2, 4 ... *`) and `.roll(*)`. A strict force of one used to
stop at a 100000-element prefix and return it as the whole list. So
`(1..*).iterator.push-all(@o)` (once the iterator pulls through the lazy list)
and `@a.iterator.push-all(@o)` over `my @a = 1..*` filled `@o` with a prefix
and returned, a wrong answer for an infinite source (#10846).

A strict force of such a list now throws `X::Cannot::Lazy`, the same verdict a
strict force of an infinite `.map`/`.grep` pipe reaches. The VM force path and
the tree-walk bridge agree. The Iterator protocol's full drains (`push-all`,
`sink-all`) propagate the error instead of reporting the pulled prefix as the
end of the source.

The front mutators `shift`, `unshift`, `prepend` and `splice` relied on that
capped prefix. Now they reify only the elements they touch, run the ordinary
Array method on that prefix, and stitch the result back in front of the live
generator. So `my @a = 1..*; @a.shift; @a.unshift(0)` leaves `@a` lazy, as in
Rakudo. A `splice` that would reach the end of the list throws
`X::Cannot::Lazy`. Sinking a lazy `@`-array is a no-op, like `Array.sink`, so
`@a.unshift(0);` in sink context does not force the list either.
