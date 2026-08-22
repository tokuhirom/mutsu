# `IO::Spec::Cygwin.is-absolute` doesn't recognize a Win32-style drive path (`C:\foo`) as absolute

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Type/IO/Spec/Cygwin.rakudoc:80`).

## Root cause hypothesis

`IO::Spec::Cygwin` inherits from `IO::Spec::Unix` but additionally treats Windows-style drive
paths (`C:\foo`) as absolute, since Cygwin paths can be either POSIX-style (`/foo`) or
Win32-style (`C:\foo`):

```raku
say IO::Spec::Cygwin.is-absolute: "/foo";        # True
say IO::Spec::Cygwin.is-absolute: "/\x[308]foo"; # True
say IO::Spec::Cygwin.is-absolute: ｢C:\foo｣;      # True
say IO::Spec::Cygwin.is-absolute: "bar";         # False
```

mutsu gets the first, second, and fourth cases right, but returns `False` for the Win32-style
drive path case — its `.is-absolute` implementation for `IO::Spec::Cygwin` likely just reuses
the POSIX-only check (leading `/`) without also recognizing the `<letter>:\` / `<letter>:/`
drive-prefix form that `IO::Spec::Win32` (and Cygwin, per this doc) already needs to detect.

## Minimal repro

```raku
say IO::Spec::Cygwin.is-absolute: "C:\\foo";
```

- `raku`: `True`
- `mutsu` (`target/debug/mutsu`): `False`

## Affected files (starting point)

- `IO::Spec::Cygwin`'s `.is-absolute` method implementation — likely near the
  `IO::Spec::Win32`/`IO::Spec::Unix` implementations (grep for `is-absolute` and `Cygwin` in
  `src/runtime/` or wherever `IO::Spec::*` classes are implemented) — needs to also match the
  drive-prefix pattern that `IO::Spec::Win32.is-absolute` presumably already recognizes.
