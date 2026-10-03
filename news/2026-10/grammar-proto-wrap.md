# Wrapping a grammar proto token or its `:sym` candidates

`.^find_method('p:sym<a>').wrap(...)` now runs the wrapper when a grammar reaches the candidate
through its proto token, in both regex engines (the compiled engine bridges a wrapped candidate,
as it already did for a wrapped plain rule), and `.^find_method('p')` on a `proto token` returns
the proto instead of `Mu`, so the proto itself can be wrapped. Closes #11151.
