# A proto's candidates are ranked by their first character when that decides it

A call to a proto token ranks its candidates by running one NFA over all of
them (ADR-0046). #19391 let an LTM alternation skip that run when the
character in front of it can begin at most one branch. A proto now takes the
same shortcut: a candidate whose *gate* (the set of characters a match of it
can begin with, seen through rule calls) rejects the character at the
position is the "cannot match here" verdict the ranking already filters on,
so it is left out without a run, and when at most one candidate is left it is
tried as is.

Two things widened the gates so the shortcut applies where it matters:

- Each root now has a gate of its own. Before, one root without a gate (a
  candidate that can match empty, or begins with a code block or `<.ws>`)
  disabled the gates of all the others.
- A gate covers an `:i` leaf too, with the prefilter's case-fold first set
  (ADR-0099), which folds ASCII and admits every non-ASCII character. A run
  guard still does not. YAMLish's `Schema::Core` declares most of its
  `element` candidates with `:i` (`null`, `yes`, `no`, `hex`, `oct`, `inf`,
  `nan`), so at a plain scalar like `value 1` every candidate was ranked by a
  full run before; now none is.

One warm `load-yaml` of `bench-yaml-parse-big`'s document, `MUTSU_JIT=off`,
callgrind: 123.06M -> 122.49M instructions (#12917).
