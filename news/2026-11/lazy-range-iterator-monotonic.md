# `(1..*).lazy.iterator` is monotonically increasing

The iterator of a lazy range (`Range.lazy`, an identity index pipe over the
range) answered `is-monotonically-increasing` `False` and `is-deterministic`
`False` (it was read as a `.map` stream); like rakudo's it is both `True`.
