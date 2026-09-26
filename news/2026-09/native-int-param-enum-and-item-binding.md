# Native-int parameters unbox enums and itemized arguments, on methods too

Binding to a native-int parameter (`int`, `int8`, `uint32`, ...) now coerces
the argument the same way on every binder:

- An Int-valued enum binds its integer: `sub f(uint8 $n) {...}; f(E::Two)`
  binds `2`, not `E::Two`. Before, only `Bool` was unboxed. `my int $x = Two`
  and native array stores follow the same rule.
- An itemized argument (`f(my $ = 200)`) binds the value it holds. The
  light-call path used to reject it with a type-check failure.
- A method's native-int parameter wraps, unboxes and range-checks its
  argument. Before, the method fast path bound the argument unchanged, so
  `method m(int8 $n)` returned `200` for `200`.

The issue also asked what Rakudo's wrapping rule on bind actually is. The
argument wraps to the declared width, which mutsu already did. Rakudo's
apparent exceptions (`sub g(uint8 $n) { $n }; g(300)` → `300`) come from its
optimizer inlining a *literal constant* argument into a trivial body. Passing
the same value through a variable, or doing anything with the parameter,
gives the wrapped `44` (issue #9533).
