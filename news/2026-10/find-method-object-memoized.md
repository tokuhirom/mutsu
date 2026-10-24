# `.^find_method` / `.^lookup` answer from a per-class memo

`$c.^find_method("foo")` rebuilt a Method object on every call (attribute map,
Signature, callable: ~34k instructions) and so never returned the same object
twice, where rakudo's `===` is True. A class receiver's `.^lookup` /
`.^find_method` answer is now memoized per `(class, method)` and cleared with
the other method caches whenever the registry's method generation changes
(`.wrap`, `.^add_method`, `augment`, role composition), so repeated lookups
return one object and cost a hash probe. The issue's repro drops from about
126k to about 77k instructions per call; the rest is the generic method-call
path in front of `.^` (see #14853).
