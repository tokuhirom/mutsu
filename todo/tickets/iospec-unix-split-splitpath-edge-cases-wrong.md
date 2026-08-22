# `IO::Spec::Unix.split`/`.splitpath` mishandle all-slash, empty, and single-dot paths

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Type/IO/Spec/Unix.rakudoc:230`
and `:291`).

## Root cause hypothesis

`IO::Spec::Unix.split(...)` and `.splitpath(...)` compute the directory/basename split
correctly for normal paths, but mishandle a few edge-case inputs: an all-slashes path
(`'///'`), the empty string, and a bare `'.'`. In each case mutsu appears to fall back
to treating the whole (or a truncated) input as the "dirname" component and leaves the
basename empty, where raku treats a bare `.`/all-slash input specially (keeping `.`/`/`
as the basename too, or as the sole path component depending on the method).

## Minimal repro

```raku
IO::Spec::Unix.split('///').raku.say;
IO::Spec::Unix.split('').raku.say;
IO::Spec::Unix.splitpath('.').raku.say;
```

- `raku`:
  ```
  IO::Path::Parts.new("","/","/")
  IO::Path::Parts.new("","","")
  ("", "", ".")
  ```
- `mutsu` (`target/debug/mutsu`):
  ```
  IO::Path::Parts.new("",".","")
  IO::Path::Parts.new("",".","")
  ("", ".", "")
  ```

Every other case in the doc's `.split`/`.splitpath` example tables (normal paths,
trailing slash, `'./'`, Windows-style `C:/foo/bar.txt`) already matches raku exactly —
only these three edge-case inputs diverge.

## Affected files (starting point)

`src/runtime/methods_call_dispatch.rs:1517` (`"splitpath"` dispatch arm) and wherever
the sibling `.split` method for `IO::Spec::Unix` is implemented (grep for
`"IO::Path::Parts"` / `Path::Parts` construction) — likely the same underlying
dirname/basename decomposition helper feeds both methods.
