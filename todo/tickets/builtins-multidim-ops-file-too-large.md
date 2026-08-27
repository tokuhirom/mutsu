# `src/runtime/builtins_multidim_ops.rs` is well over the 500-line file-size convention

Noticed while landing `news/2026-08/multidim-value-adverb-hole-shape.md`: the file was already 952
lines before that fix (it grew to 978 after, mostly from doc comments explaining the corrected
adverb-shape rules). CLAUDE.md's conventions section says "Keep each Rust source file under 500
lines. When a file exceeds 500 lines, split it into smaller modules immediately -- do not defer",
but this file has clearly been over that threshold for a while and the violation predates the fix
above by a wide margin, so it was left alone rather than folding an unrelated large structural split
into a targeted bug-fix PR.

## Scope

The file holds seven `pub(super) fn` handlers on `impl Interpreter` for multidim (`;`-separated)
subscript adverbs (`__mutsu_multidim_adverb`, `__mutsu_multidim_subscript_adverb` (+ its `_multi` and
`_dyn` helpers), `__mutsu_multidim_exists_adverb` (+ its `_multi` helper and two `positional_exists_items`
/ `nested_exists_slice` support functions), `__mutsu_multidim_delete`, and a few small private helpers
(`check_shaped_index_bounds`, `writeback_multidim_var_to_local`, `resolve_multidim_indices`,
`multidim_empty_list`, `multidim_missing_result`). A natural split along the existing builtin-name
groupings (subscript-adverb family vs exists-adverb family vs delete) would land each new module
comfortably under 500 lines; verify with `make test` + a roast sweep of the same `S09-*`/`S32-array`/
`S32-hash` files used to verify the parent fix, since a split needs to preserve the exact `impl
Interpreter` method visibility (`pub(super)`) that `src/runtime/builtins.rs`'s dispatcher relies on.
