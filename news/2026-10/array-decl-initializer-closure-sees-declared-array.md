# A closure in `my @a = ...` / `our @a = ...` reads the declared array

`my @h = 0, {@h[$++] + 1} … *;` gave `(0 1 1 1)`: the closure captured `@h` as an
upvalue snapshot taken before the declaration's store, so it read the empty array
(only single-letter uppercase names such as `@H` escaped, because they are never
captured). The compiler now records the `@`/`%` names whose initializer it is compiling
and leaves them out of the closures' captured variables (and upvalue promotion), so the
closure reads the declared container by name. Found with Math::Sequences.
