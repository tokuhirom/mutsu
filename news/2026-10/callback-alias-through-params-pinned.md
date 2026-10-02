# Callback and array-element aliases through routine parameters are pinned

Issue #9260 reported that a callback passed through a routine's `&code`
parameter lost its writable aliases: `apply({ $_++ }, 0..3)` returned
`(0, 1, 2, 3)`, and `@values.grep(&code, :k)` with an incrementing callback
left the caller's array untouched. That also made `List::MoreUtils`'s
`pairwise` reject `-> $a is rw, $b is rw { ... }`.

Re-measured on current `main`, every shape in the issue already matches
Rakudo. The rw-parameter aliasing work since the issue was filed (ADR-0109's
light-call alias binding and the writeback-coherence fixes) covered it, and
the `List::MoreUtils` 0.0.10 ecosystem record is green at 60/60 files.

`t/routines/signature/callback-aliases-through-code-and-array-params.t` now
pins the issue's repro together with the `pairwise`, `for ... is rw` and
`.map(&c)` shapes, so a later regression shows up as a test failure.
