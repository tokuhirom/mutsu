# `chdir :!d, $path` takes the adverb as the path, and `:!d` does not skip the existence test

Found while fixing the sibling bug in `indir` (see
[news/2026-08/reduce-metaop-numeric-coercion-bypassed.md](../../news/2026-08/reduce-metaop-numeric-coercion-bypassed.md),
which had to fix `indir :!d` to land). `indir` and `chdir` share almost
identical bodies in `src/runtime/builtins_io_dir.rs`; the `indir` half was
fixed, the `chdir` half was left because it performs a *real* process chdir and
deserves its own verification pass.

## Two distinct bugs, same call

```raku
say (chdir :!d, "definitely-not-here");
# raku : "<cwd>/definitely-not-here".IO
# mutsu: Failed to chdir to 'd\tFalse': no such file or directory
```

1. **The adverb is taken as the path.** `builtin_chdir` does not skip `Pair`
   arguments when picking its positional path, so the `:!d` pair itself is
   stringified into `"d\tFalse"` and used as the target. `builtin_indir` gets
   this right — it has an explicit `if matches!(arg.view(), ValueView::Pair(_, _)) { continue; }`
   in its argument loop. `builtin_chdir` needs the same.

2. **`:!d` must skip the existence test.** As with `indir`, the `:d` adverb
   (default `True`) is what requests the directory test, and existence is part
   of it. rakudo's `chdir :!d, $nonexistent` succeeds and returns the
   `IO::Path`. mutsu's `builtin_chdir` tests `!absolute_target.exists()`
   unconditionally.

Verify what the *process* chdir should do in that case before implementing:
Raku's `$*CWD` is a virtual working directory, so rakudo may well not be
issuing a real `chdir(2)` at all here. That distinction is exactly why this was
not folded into the `indir` fix.

## Affected files

- `src/runtime/builtins_io_dir.rs` — `builtin_chdir` (the argument loop, and
  the `!absolute_target.exists()` guard). `builtin_indir` in the same file is
  the corrected shape to copy.
- `src/runtime/builtins_io.rs` — `parse_io_requirements` already decodes the
  adverbs correctly; `has_required_mode_bits` was fixed to be vacuously true
  when no mode bits are requested, so it no longer rejects a nonexistent path.

## Pin

`roast/S32-io/chdir.t` passes today (it does not exercise `:!d`), so a new
`t/` test is needed.
