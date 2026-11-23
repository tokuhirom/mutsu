# `List.pick` with a negative count dies

rakudo's `List.pick` coerces its count to `UInt`, so `<a b c>.pick(-1)` dies
with `X::OutOfRange`:

```
Coercion to UInt out of range. Is: -1, should be in 0..^Inf
```

mutsu clamped the count to zero and answered `()`. A List, an Array or a Hash
now dies with the same message. A Range, Set or Bag still answers an empty
pick, as it does in rakudo.
