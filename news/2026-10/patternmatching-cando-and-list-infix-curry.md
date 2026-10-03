# PatternMatching: list-infix Whatever-currying and `.cando` matching

Drawing `PatternMatching` from the ecosystem roulette turned up four gaps.
With all four fixed, its `t/01-pattern-matching` passes 20/20 under mutsu; it
passed 6/20 before.

- **Whatever-currying a list infix.** `* op a, b` now curries over the whole
  comma list when `op` is a user infix at list-infix precedence
  (`is equiv<Z>`), as in `@x.map: * match_pattern -> ... , -> ...`. Before, the
  curry closed over `a` alone and the rest of the list became separate
  elements.
- **Positional Pairs in `.cando`.** `.cando(\($pair))` now matches a Pair held
  in a variable as a positional argument. A routine or block without an
  invocant now goes through the sub matcher, where a positional Pair is
  positional; before, the method matcher read it as a named argument.
- **Objects in a sub-signature.** An object now destructures through its
  default `.Capture`: its attributes are named parts, with no positional part.
  So `multi f(C (:$a))` matches `C.new`.
- **Renaming onto a destructuring target.** `:value([$b, $c])` now hands the
  Pair's value to the `[$b, $c]` target during dispatch. Before, dispatch took
  the value apart one level too deep. An itemized Pair (`(e => [5, 6]).item`)
  also destructures by name now.
