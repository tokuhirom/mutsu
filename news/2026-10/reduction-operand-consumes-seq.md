# Reductions and zip/cross consume their Seq operand

A reduction (`[+] s`) now iterates a `Seq` operand the way Rakudo does. An
uncached Seq is consumed, so a later `s.elems` raises `X::Seq::Consumed`, and a
Seq that was already consumed raises at once instead of being read again. This
holds for a `.map` Seq as well as a gather. `[max]` and `[min]` leave the Seq
readable, because Rakudo calls the operator's list candidate, which caches it.

A plain reduction over a gather now reads the gather through its `Seq` body
rather than through the gather's inner coroutine list (ADR-11678 §8.5). The
triangle form and the zip and cross metaoperators still stream an infinite
gather.

The zip and cross metaoperators consume a `.map` Seq too, and raise on a
consumed one. They now read an itemized Seq (`$s Z 1`) as a single element, as
Rakudo does.
