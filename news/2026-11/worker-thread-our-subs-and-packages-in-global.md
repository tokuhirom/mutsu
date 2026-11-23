# `our sub`s and packages a worker thread declares are reachable from the main thread

On rakudo an `our sub` or a top-level `package` declared on a worker thread
(an `EVAL` inside a `start` block, say) becomes a member of the program's
`GLOBAL`, and the main thread can reach it once the worker is done. In mutsu
the routine reached the world `our` table (ADR-17216), but three lookups still
read only the main thread's own tables:

- the package stash, so `Pkg::.keys` and `Pkg.WHO.keys` left the routine out,
  and `Pkg::<&f>` did not find it;
- `EVAL`'s undeclared-name check, so a bare `Pkg` died with
  "Undeclared name";
- the bareword lookup, so a bare `Pkg` evaluated to the string `"Pkg"`.

Each lookup now also reads the routines and package declarations other
threads published to the world tables. A name that no thread declared is
still reported as undeclared.
