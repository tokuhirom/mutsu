# Data::Reshapers: five interpreter fixes

Data::Reshapers had 8 of 15 test files at parity. Five separate gaps were
behind the other seven, and all 15 files now pass under mutsu.

- **`.isa` with a parameterized type.** `my Hash @a; @a.isa(Array[Hash])` was
  False: the `isa` builtin compared only nominal names. It now compares the
  container's own parameterized type (`Array[Hash]`, `Hash[Int]`,
  `Hash[Any,Int]`). That type comes from a new pure helper,
  `embedded_container_type_name`, which `.WHAT` now shares.
- **A typed hash assigned inside a closure.** `my Hash %r; lives-ok { %r = f() }`
  left `%r` a plain `Hash`. The by-name `SetGlobal` path only coerced the
  values. It now goes through `hash_container_writethrough_value`, the `%`
  twin of the array path, which also keeps the `Hash[T]` metadata.
- **`xx` / `x` counts.** `'W' xx Set(<a b>)` produced nothing. A Set, Hash,
  Bag, Mix, Bool, enum or Range count is now numified with the shared
  `coerce_to_numeric`, so it repeats `.elems` (or the total weight) times.
- **Four or more `Z` operands in parentheses.** `(1 Z 2 Z 3 Z 4)` came out as
  `((1, 2, 3), 4)`. The paren-list normalizer rewrote the inner chain into a
  `zip(...)` call before checking whether the outer level continues the same
  chain. It now checks the operands as written first, so the whole chain is one
  n-ary zip.
- **`%h.values[*]`.** The Seq `.values` returns keeps its source's element
  cells and skips the List normalization, and it had no `*` subscript arm, so
  the subscript returned `Nil`. Data::TypeSystem's `has-homogeneous-type` reads
  exactly that, so every hash deduced as an `Assoc` instead of a `Struct`.
