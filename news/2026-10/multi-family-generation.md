# A multi family's caches survive another family's registration

The multi dispatch memos (the candidate probe, the type-cacheability verdict,
the resolved winner, the light-call entry and the bare multi plan) used to be
tagged with the version of the whole routine registry. A block that declares a
routine of its own changes that version on every entry and exit, so a loop
around such a block re-ran the full candidate walk of every *other* multi it
called on every iteration.

Each base name now has its own generation, bumped when one of its registry
keys changes (ADR-12529 §2.4, phase 2 slice 7). An entry is still served on
the registry version first, at no extra cost; when that version moved, the
entry is served again as long as its own family did not change.
`for ^3000 { { multi g(Int $z) { $z }; f($i) + g(1) } }` now resolves `f`
once instead of 3000 times.
