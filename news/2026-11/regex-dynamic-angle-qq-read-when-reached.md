# `$*dyn`, `<$var>` and `"..."` in a regex are read when the cursor reaches them

Rakudo reads an interpolation in a regex at the moment the cursor reaches
it, so a code block earlier in the pattern can change what it matches. mutsu
already did that for plain `$var` (#14510's earlier slices); three shapes
still read the value before the match started:

- a dynamic variable: `/ x { $*v = "b" } $*v /` and a grammar token reading a
  `$*v` its caller or an earlier block set are now lowered to the match-time
  `VarInterp` atom instead of being spliced into the pattern text;
- `<$var>` after a code block is now a match-time `VarAssertion`;
- a `"..."` atom's compiled qq thunk now runs each time the atom is reached
  (`"aaa" ~~ / [ "{ $n++; 'a' }" ]+ /` evaluates it four times, as in
  rakudo) instead of once when the regex's scope is installed. A body of
  compile-time constants is still evaluated once, as part of the declarative
  prefix.

Running the thunk at match time exposed a parse-cache collision: the text
scan an atom falls back to was memoized under the same text as a pattern
made of nothing but that atom, so the second match of `/"{ Q.^name }"/`
silently used the fallback and failed. A fallback parse no longer enters
the parse caches.
