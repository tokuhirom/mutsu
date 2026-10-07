# A `does Sequence` class with its own iterator behaves like a Seq (SeqSplitter)

`SeqSplitter` (a zef distribution built on `does Sequence` plus a `has $.iterator`
accessor) went from 1/2 to 2/2 baseline files at parity. Four interpreter gaps were
behind it:

- A class that composes `Sequence` and supplies `iterator` (as a method or a
  `has $.iterator` accessor) now answers `.list`, `.List`, `.Seq`, `.elems`, `.Str`,
  `.gist`, `.join` and friends by draining that iterator, and stringifies through it
  in `eq`, interpolation and `~`.
- A multi method signature naming a role declared inside the same class
  (`role SI` nested in `class Outer`) is now ranked from the declaring package, so it
  no longer loses to a wider `Iterator` candidate.
- A role that composes another role is now a narrower constraint than the composed
  role for a class that composes it, whichever candidate is declared first.
- `.^attributes.first('$!name')` matches an `Attribute` by its name.
