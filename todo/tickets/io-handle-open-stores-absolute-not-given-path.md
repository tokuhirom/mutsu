# `.IO.open` stores the CWD-absolutized path instead of the path as given

Found by the doc-diff harness (`docs/doc-diff-backlog.md`, `Type/IO/Handle.rakudoc:761`).

## Root cause

`IO::Handle::open` (`src/runtime/native_io/io_handle.rs`, the `"open"` match arm)
resolves the requested path to an absolute path via `self.resolve_path(&path_str)`
(`src/runtime/io_env.rs::resolve_path`, which always joins a relative path onto
`$*CWD`) **before** opening the file, and then passes that already-absolutized
`PathBuf` straight into `open_file_handle` (`src/runtime/handle_open.rs`). There,
`open_file_handle` stores `Self::stringify_path(path)` — the absolutized path — as the
handle's `path` attribute (`src/runtime/handle_open.rs:534`). Rakudo instead opens the
file using the resolved/absolute path internally but keeps the *original* path string
(relative or absolute, whatever the caller passed) as the handle's `.path`/`.Str`
representation.

## Minimal repro

```raku
say "foo".IO.open.Str;
```

- `raku`: `foo`
- `mutsu` (`target/debug/mutsu`): the full absolutized path, e.g.
  `/home/.../foo`

Also affects `.path` (which raku gists as `"foo".IO`, mutsu gists as
`"/home/.../foo".IO`). Confirmed the plain `.IO` (no `.open`) already keeps the
relative path correctly (`"foo".IO.Str` prints `foo` in both); the absolutization only
happens through the `.open` path.

## Affected files (starting point)

- `src/runtime/native_io/io_handle.rs` (`"open"` arm, ~line 195-263) — should keep the
  original (as-given) path string for the handle's `path` attribute, using the
  resolved/absolute `PathBuf` only for the actual `OpenOptions::open()` filesystem
  call.
- `src/runtime/handle_open.rs::open_file_handle` (~line 471-540) — currently stores
  whatever `Path` it's given; may need an extra parameter (or the caller passing an
  already-corrected display path) to decouple "path used to open" from "path stored
  for display".
