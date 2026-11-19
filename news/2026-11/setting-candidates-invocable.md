# A setting routine's `.candidates` can be called

`&infix:<+>.candidates[5](2, 3)` answers `5`, as in Rakudo. The candidate
code objects mutsu builds for a built-in routine (`builtin_candidates.rs`)
carried the candidate's signature and declaration site but a body that died
with "Built-in candidate of infix:<+> is not directly invocable", so code that
walks a family's candidates and calls each one, including a family a program's
`multi` joins (#17923), died on the setting's entries.

A sub candidate's body now calls the built-in routine through `CORE::` on the
arguments its own signature bound (positional, sigilless, slurpy, and named
ones passed on when given), so the call reaches the core implementation and
never re-enters a program `multi` of the same name. A candidate whose
signature has a parameter that cannot be named in the call (an anonymous
`Int:D`, a renamed named parameter, a literal or a sub-signature) and the
method candidates keep the old body (#18009).
