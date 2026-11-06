# Typed Array subclass `.raku` and specialized-role `.^roles`

`P[Int].new(1, 2).raku` (`class P is Array`) now prints `Array[Int].new(1, 2)` without a
`$(...)` wrapper, as rakudo does, and `.^roles` of a specialized parametric role substitutes
its type parameters (`R[Str].^roles` is `Positional[Str]`, `Array::Typed[Str].^roles` too)
instead of listing the declaration's `Positional[T]` (#16658).
