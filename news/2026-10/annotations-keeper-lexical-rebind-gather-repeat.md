# annotations Keeper: gather `xx *`, `is raw` loop parameters and lexical-sub rebinds

Three interpreter gaps found running the `annotations` distribution's `Keeper` container:

- `gather do { ... take ... } xx *` ran the repeat from Rust, which cannot suspend the gather at a
  `take`, so it never ended; a sunk `EXPR xx N` in a gather body is now a `for ^N { EXPR }` loop.
  `(1 xx *) Z <a b>` also no longer curries into a WhateverCode.
- A `for` pointy parameter marked `is raw` is writable (a Proxy element runs its STORE).
- A nested sub's `$x := ...` is now seen by an earlier-declared sibling that captured the same
  variable (the shared cell becomes the binding cell in place), and a Proxy whose FETCH answers
  that binding cell yields the value in method-call position. `IntStr.new` reads through an
  `is raw` argument's container.
