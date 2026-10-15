# `.Slip` and `slip(...)` of a lazy list stay lazy

A Slip reifies on demand in Rakudo. In mutsu, `.Slip` of a lazy list copied its
(empty) cache or hung, and `slip(1..*)` was a one-element Slip holding the
Range. Now `.Slip` and `slip(...)` of a genuinely lazy list — an infinite
Range, a `.lazy` gather, a lazy `.map`, a scan — give a lazy-tail Slip: the
Slip carries the lazy list whole, the shape `|EXPR` already gave a lazy operand.
`(1..*).Slip.head(3)`, `.is-lazy`, `[3]`, `.map`, `for` with `last`, and
`(0, slip(1..*)).head(3)` all read it lazily; it is still a `Slip`, gists as
`(...)`, and `.elems` refuses it with `X::Cannot::Lazy`.

A plain `gather`'s Slip is still pulled whole (so an infinite gather's `.Slip`
still hangs): ADR-11678 §8.6 records why it needs the Slip flatten sites routed
through one interpreter-side chokepoint first (#13117 stays open for it).
