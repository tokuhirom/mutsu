# A filled free-variable slot is read without a copy of the binding

`GetLexFree`, the slot read of a sub's free variables, cloned the slot's
binding before reading through it, and resolved the variable's name to test
guards that only matter while a `for` loop binds a parameter by name or an
atomic variable exists. It now borrows the binding and tests the
per-interpreter flags first, resolving the name only when one of those can
apply.

A nested sub's loop reading an outer `@out` and `$comma` went from 4,958 to
4,482 instructions per iteration (4,128 for the same loop on locals), a
top-level sub's loop over unit lexicals from 5,181 to 4,623 (4,207), and
bench-json-fast-encode's JIT row dropped 1.7%. An outer-lexical read now
costs within 10% of a local one, the first close condition of #18274.
