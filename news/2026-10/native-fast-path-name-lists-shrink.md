# The native method fast path loses its name lists for `Supply`, adverbs and `AT-POS`

The VM's native fast path declined a fixed list of twenty `Supply` method
names, sent `squish` and `tail` to the interpreter by name, and answered the
`:i`/`:m` forms of `contains`/`starts-with`/`ends-with`/`substr-eq`,
`base(..., :no-trailing-zeroes)` and `.AT-POS` from interceptors in front of
the method table (ADR-11276 §9.168). A `Supply` now leaves the fast path by its
class, and the rest answer through their rows; `Rat.base` declares
`:no-trailing-zeroes` as Rakudo's `Rational.base` does.
