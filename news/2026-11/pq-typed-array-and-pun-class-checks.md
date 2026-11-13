# Typed arrays on the AT-POS fast path; a parameterized role's class is memoized in type checks

Three general costs on Concurrent::PriorityQueue's `push` path:

- A typed or defaulted array (`TYPE @!keys` in a parameterized role) never
  took the subscript fast path, because its element type and default change
  what a miss reads. A present ordinary element reads as itself either way,
  so the fast path now answers it and leaves every miss (a hole, an index
  past the end, a stored `Nil`, a stored type object) to the general path.
- The `(class, type)` memo for instance type checks declined any type name
  containing `[`, which includes the class a parameterized role puns into
  (`Concurrent::PriorityQueue\0default[...]`, what `::?CLASS:D:` names in its
  multi candidates). A name that is itself a registered class is now admitted;
  role parameterizations (`R[Int]`) still take the full check. That check was
  ~23k instructions per candidate per call.
- A built-in aggregate, pair or code value answers `Any` / `Mu` / `Callable` /
  `Pair` constraints (an untyped or `&`-sigil parameter's check) without the
  full type-matching path.

Single-thread `@a.push` of 32000 keys through the class: 13.8 s -> 12.6 s;
`t/02-concurrent.rakutest`: 47 s -> 42 s (profiling build, 4-core box).
