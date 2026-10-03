# The native method row table tolerates a repeated row

`src/builtins/native_method_row_table.rs` may now list the same `(owner, name)`
row more than once. Readers go through `rows()`, which keeps the first row of
each key, so a repeat changes neither the lookup nor `.^methods`. A test still
fails when a key is repeated with a different arity or different flags.

The old `raw_rows_have_no_duplicate_keys` test turned every pair of sibling PRs
that added the same row into a red `main`. On 2026-10-03 the cleanups then
removed every copy and about twenty PRs each pushed their own fix. With this
change, two PRs adding the same row merge cleanly.
