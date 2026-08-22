# `IO::Path ~~ :w`/`:r`/`:x` smart-match ignores effective (per-user) permission

Discovered via the doc-diff harness on `raku-doc/doc/Type/IO/Path.rakudoc` (around line 561).

## Repro

As a non-root user, where `/` is mode `755` and owned by `root`:

```
say '/'.IO ~~ :w;
```

- raku: `False` (the current user cannot actually write to `/`)
- mutsu: `True`

Confirmed the plain `.w` **method** is already correct: `'/'.IO.w` gives `False` in both raku
and mutsu. The bug is specific to the `~~ :w` smart-match dispatch path.

## Root cause (confirmed)

`src/vm/vm_smart_match.rs`, `io_path_file_test_result()` — the `"w"`/`"r"`/`"x"` arms check the
raw mode bits (e.g. `mode & 0o222 != 0`, true if **any** of owner/group/other has the write bit
set) instead of performing an actual effective-access check for the current user. The correct
logic already exists and is used by the `.w`/`.r`/`.x` *methods* via `libc::access()` in
`src/runtime/native_io/helpers.rs:64` — the smart-match path duplicates the file-test logic
independently instead of delegating to the same helper.

## Suggested fix

Have `io_path_file_test_result()` in `src/vm/vm_smart_match.rs` call the same `libc::access()`
based helper that `src/runtime/native_io/helpers.rs:64` already uses for `.w`/`.r`/`.x`, instead
of re-deriving the answer from raw mode bits.

## Suggested test

A regression test needs a file/dir with a mode that has the bit set for some class other than
the current effective user (e.g. a `chmod`-created file the test creates and owns, checked
against `:w` after `chmod 0o444`), verified against raku's actual `access()`-based semantics.
