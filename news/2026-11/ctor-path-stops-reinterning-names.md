# The construction path stops re-interning names

`bench-ctor` was paying for names it already had as symbols.

- Every `bless` scanned the class's MRO for the built-in classes that need a
  native payload: `Int`, `Num`, `Rat`, `Str`, `Attribute`, `Capture`,
  `Promise`, `Channel`, `Supplier`, the repository classes and
  `IterationBuffer`. Each test compared a `Symbol` with a string literal, so
  every MRO entry cost an `as_str` and a `memcmp`. These are now well-known
  symbols, and each test is an integer compare.
- Four steps of a construction looked up the class's cached ctor plan by
  `Symbol::intern(class_name)` although `bless` holds the class symbol. They
  now take the symbol.
- Every `TWEAK` or `BUILD` phase call interned the class and the method name
  again, and copied the method name into a `String` for the `samewith`
  context. A phase step now carries its class interned at plan build, and
  the method name is borrowed as a `'static` string.

Measured with callgrind (`MUTSU_JIT=off`): `bench-ctor` goes from 1,235.3M
to 1,216.2M instructions (-1.5%) and from 714,715 to 702,552 allocations.
`Symbol::intern` drops from 30.3M to 21.4M instructions and `Symbol::as_str`
from 34.7M to 26.9M.
