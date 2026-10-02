# Value's identity, comparison and list-expansion core moves below the runtime

Slice 6 of the layer decomposition (#10779) cut the upward references from the
lower layers from 86 to 76. The pure helpers that `Value` itself leans on now
live in `src/value/` instead of `runtime::utils` / `builtins`:

- identity: `values_identical` / `values_same_object` (`value/identity.rs`),
  `IdentityIndex`, the `.WHICH` key `value_which_key` (`value/which_key.rs`) and
  the QuantHash key helpers;
- numeric comparison and coercion: `compare_values` (`value/compare.rs`),
  `coerce_to_numeric` and the radix parser (`value/radix_numeric.rs`), the
  rational-parts helpers (`value/rat_parts.rs`), `Str.Numeric`'s parser
  (`value/str_numeric.rs`), `Version` ordering (`value/version_cmp.rs`);
- list shape: `value_to_list` (`value/to_list.rs`, with its `GenericRange` arm in
  `value/to_list_range.rs`), `flat_val`, `.tree`'s `tree_to_depth` and
  `is_infinite_range` (`value/flat.rs`), and the finite half of
  `coerce_to_array` with element itemization (`value/array_coerce.rs`);
- string `.succ`/`.pred` (`value/str_increment.rs`) and the civil-date
  constructors (`value/temporal_core.rs`).

`runtime::utils` and `builtins` re-export the moved names, so callers above are
unchanged. One move went the other way: the `LazyList` pipe constructors
(`new_pipe`, `new_index_pipe`, ...) route every source through
`runtime::unbounded_range`, which steps a range with the `.succ`/`+` builtins,
so their `impl LazyList` block now lives in `runtime/lazy_pipe_ctors.rs`.
`coerce_to_array` likewise keeps only its unbounded-range case in the runtime.
