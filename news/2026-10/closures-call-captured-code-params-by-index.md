# A closure calls its captured `&`-parameter by index

A combinator such as `sub apply(&f, &p) { -> @x { &p(@x) } }` returns a
closure that calls the routine it was handed. Each of those calls used to
resolve `&p` by name, walking the env of whichever frame happened to be
running the closure.

The compiler now gives such a call the closure's upvalue index. When the
closure is created, a readonly `&`-parameter (one without `is copy`, `is rw`
or `is raw`) is captured by value, because nothing can rebind it while its
routine runs, and the call reads that binding directly. A `my &f`, which can be
reassigned later, still resolves by name. This is part of ADR-12529 phase 2.
