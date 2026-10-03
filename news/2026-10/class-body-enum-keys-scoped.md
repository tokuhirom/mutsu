# Enum keys declared in a class body stay in the class; `array` is not a List

PDF::Grammar's suite now passes all ten of its test files; it used to pass
four. It needed three fixes.

- **An enum declared in a class or grammar body keeps its keys there.**
  PDF::Grammar declares `enum AST-Types <array body bool ...>` inside
  `grammar PDF::Grammar`. The key `array` leaked into the enclosing scope as a
  bare term, so every later `array[uint64].new(...)` in the program indexed
  that enum value and died with "Unable to call postcircumfix:<[ ]> with a type
  object". When the body exits, its enum keys are now dropped from the outer
  scope, just as the enum type's short name already was.
  - The body's own methods still see the keys. A bare term is resolved
    against the routine's *lexical* package, so a key that shares a builtin
    type's spelling (`array`) still wins inside the class. That lookup also
    reads the top-level package-symbol table.
- **A native `array[T]` is not a `List`.** `array`'s MRO is
  `array, Cool, Any, Mu`. Both the static and the value-aware type checks had
  claimed native arrays for `List`.
- **The multi-dispatch cache keys a native array by its own type.** It used
  to key one exactly like an `Array`. So once `(List:D $a)` had won for an
  `Array`, the next native array was sent there too, and binding failed
  (PDF::Grammar::Test's `json-eqv` candidates).
