# `===` on objects stops re-deriving whether a class overrides `WHICH`

`===` is `.WHICH eq .WHICH`, so before comparing two instances the interpreter
asks whether either class overrides `WHICH`, with a `method WHICH` or with a
`has $.WHICH` accessor. The method probe was memoized per class. The accessor
probe was not: it cloned the class's MRO and re-interned `"WHICH"` on every
operand. The class name was also resolved into a fresh `String` each time.
Both probes now go through the per-class probe memo, which the registry write
generation keeps valid, and use a well-known `WHICH` symbol.

`values_identical` also stopped allocating:

- two type objects with the same name (`$next === Node` on a linked list's
  sentinel) are identical at once, without rendering both names;
- the instance arm compares interned class names instead of resolved
  `String`s.

In Concurrent::Queue's enqueue/dequeue loop, which runs five `===` tests per
element, each test went from 5.1k to 2.9k instructions (callgrind slope,
profiling build).
