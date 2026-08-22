# `$*RAKU.compiler.verbose-config` is unimplemented

Found by the doc-diff harness batch-4 re-run (`docs/doc-diff-backlog.md`,
`Type/Compiler.rakudoc:58`).

## Repro

```raku
say $*RAKU.compiler.verbose-config;
```

- `raku`: prints ~200 lines of MoarVM/Rakudo build configuration key=value pairs
  (compiler flags, third-party library paths, kernel/distro info, etc.) — entirely
  build-environment-specific, not reproducible across machines/versions.
- `mutsu` (`target/debug/mutsu`):
  ```
  No such method 'verbose-config' for invocant of type 'Compiler'
  ```

## Priority note

This is a low-priority gap: real raku's `.verbose-config` output is exhaustively tied
to the exact MoarVM build (compiler flags, `3rdparty/` library paths, the build
machine's `uname`/distro info, etc.) that produced the running `raku` binary — mutsu
has no equivalent build-time data to report truthfully, so any implementation would
either need to fabricate MoarVM-shaped keys (misleading) or return a
mutsu-appropriate but structurally different config map (which would never match the
doc's `# OUTPUT`). Worth a stub that returns *something* (e.g. mutsu's own
build/version info under a smaller key set) so the method at least doesn't throw, but
matching raku's exact output is not a realistic goal.

## Affected files (starting point)

- Wherever `Compiler`'s other reflection methods (`.version`, `.name`, `.build-date`,
  etc.) are implemented — grep for `"Compiler"` method dispatch in `src/runtime/` or
  `src/builtins/methods_0arg/`.
