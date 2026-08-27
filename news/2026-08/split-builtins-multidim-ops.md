# `builtins_multidim_ops.rs` split into three modules, back under the 500-line convention

`src/runtime/builtins_multidim_ops.rs` had grown to 978 lines — nearly twice CLAUDE.md's "keep each
Rust source file under 500 lines" convention, and over it by a wide margin for a long time. It is
now three modules, each comfortably under the limit, split along the builtin-name groupings that
already organised the file:

| module | lines | holds |
| --- | --- | --- |
| `builtins_multidim_ops.rs` | 273 | the shared support plus `:delete` |
| `builtins_multidim_subscript_adverb.rs` | 360 | the `:v`/`:k`/`:p`/`:kv` family |
| `builtins_multidim_exists_adverb.rs` | 385 | the `:exists` family |

## What went where

The **value-adverb family** — `builtin_multidim_adverb`, `builtin_multidim_subscript_adverb`, its
private `multidim_subscript_adverb_multi` helper, and the two dynamic-`:$delete` twins
(`builtin_multidim_dynamic_adverb`, `builtin_multidim_subscript_adverb_dyn`) — moved to
`builtins_multidim_subscript_adverb.rs`.

The **`:exists` family** — `builtin_multidim_exists_adverb`, `multidim_exists_adverb_multi`,
`builtin_multidim_exists_adverb_dyn`, and the two nested-slice support functions
`positional_exists_items` / `nested_exists_slice` — moved to
`builtins_multidim_exists_adverb.rs`.

`builtins_multidim_ops.rs` keeps what all three need: `resolve_multidim_indices` (WhateverCode
dimensions, range expansion, non-Int index coercion), `writeback_multidim_var_to_local` (the
dual-store local-slot mirror), the `multidim_empty_list` / `multidim_missing_result` miss-shape
rules with their `raku`-vs-roast evidence, `check_shaped_index_bounds`, and `builtin_multidim_delete`
itself — `:delete` is not an adverb family of its own and is the only caller of the bounds check.

## Visibility

The split is otherwise mechanical: no logic changed, and every handler kept the exact `pub(super)`
visibility `src/runtime/builtins.rs`'s dispatcher relies on. Two items had to widen because a
private `fn` inside an `impl` block is private to its *module*, and their callers now live in
sibling modules:

- `resolve_multidim_indices`: private → `pub(super)`
- `multidim_empty_list` / `multidim_missing_result`: private free functions → `pub(super)`

`check_shaped_index_bounds` and each family's `_multi` helper stayed private, since each is used
only within the module it landed in. `positional_exists_items` / `nested_exists_slice` keep their
pre-existing `pub(crate)`.

## Verification

`cargo build` compiled on the first attempt; `cargo fmt` and `cargo clippy -- -D warnings` clean.
The targeted roast sweep the ticket asked for — all 57 whitelisted `S09-typed-arrays` /
`S32-array` / `S32-hash` / multidim / multislice files, including both `multislice-6e.t` files that
pin the miss-shape rules — passed (9375 tests). The full local `t/` suite passed unchanged (3495
files, 34516 tests).
