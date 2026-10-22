# Two closure sources no longer lean on the caller's binding winning

Measuring ADR-0055 slice 2 (a compiled closure's captured lexical beating a
same-named caller binding) showed which closure sources only worked because
the caller won. Both are fixed at the source, with the merge policy unchanged:

- The `substr-rw` / `subbuf-rw` Proxy closures carried a snapshot of the whole
  scope (`self.env.clone()`); they now capture only the window-length cell and
  read their source variable by name, live.
- A closure created inside `EVAL` that escapes captured a caller scalar the
  snippet cannot show to be mutated later; the unit now boxes such caller
  scalars into shared cells before it runs, so the caller's later writes stay
  visible to it (#7546).
