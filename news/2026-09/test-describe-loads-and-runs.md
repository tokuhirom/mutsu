# Test::Describe loads and runs its suites

`Test::Describe` (a `blocked_load` ecosystem record) failed to parse and then to run. Four general gaps
were behind it:

- The single-argument-rule unpack `+[Int $first, *@rest]` is now a parameter (it was only accepted
  as `*[...]`).
- A pointy block can rename a named parameter (`-> :counter(&c) { ... }`, `-> :key($k) { ... }`), and
  `&:name` works as the callable form of the `$:name` placeholder.
- A hash initializer flattens a non-itemized Hash/Map/`Foo::` stash item, so
  `sub EXPORT(--> Map()) { Foo::, "&x" => &x }` builds a Map instead of dying in the coercion.
- A `"&MAIN" => &MAIN` pair returned by `sub EXPORT` makes that routine the importing program's
  MAIN. The module's own MAIN candidates are now dropped only after the hook has run.

The suites now start and run their subtests; the remaining failures (a `:counter(&c)` alias that
receives a `Str`) are tracked separately.
