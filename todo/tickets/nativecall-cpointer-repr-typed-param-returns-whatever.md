# A `repr('CPointer')`-typed native parameter makes the call return `Whatever` instead of the declared type

## Repro

`Archive::Libarchive::Raw.rakumod` (REA `Archive::Libarchive::Raw` v0.1.5)
declares a `CPointer`-repr opaque handle class and native subs that take it:

```raku
class archive is repr('CPointer') is export { * }
sub archive_read_new(--> archive) is native(LIB) is export { * }
sub archive_read_support_filter_all(archive $archive --> int32) is native(LIB) is export { * }
```

Test usage:

```raku
my archive $a = archive_read_new();
ok {defined $a}, 'initialization';       # passes
is archive_read_support_filter_all($a), ARCHIVE_OK, ...;   # fails
```

Under `raku` this all passes. Under mutsu:

```
$ mutsu -I lib t/02-list.rakutest
ok 1 - initialization
Type check failed for return value; expected int32 but got Whatever (*)
  in block <unit> at t/02-list.rakutest line 11
```

`archive_read_new()` itself apparently succeeds (the `archive` CPointer handle
comes back defined — test 1 passes), but the *next* native call that takes
that CPointer-repr value as a parameter fails its own return-type check,
producing `Whatever` instead of running the native function and returning
`int32`. This reads as: passing a `repr('CPointer')`-typed value as an
argument to a second native call breaks that call's dispatch, and mutsu
substitutes `Whatever` rather than actually invoking the native function or
raising a clear argument-type error.

## Where found

`docs/batteries/compression.md` survey (2026-08-22), measuring
`Archive::Libarchive::Raw` (archive/zip/tar-via-libarchive battery
candidate) — every test file past `00-use`/`01-version` fails this way
(`02-list.rakutest`, `03-extract.rakutest`, `04-archive.rakutest`,
`05-archive-read-disk.rakutest`; the higher-level `Archive::Libarchive`
wrapper inherits the same failure). raku: 6/6 files (119 assertions). mutsu:
1/6 (2 assertions, `00-use`/`01-version` only — everything that actually
calls a native `archive`-taking function fails).

## Affected files

NativeCall argument-passing / dispatch, likely `src/runtime/` NativeCall
support and wherever native-call argument marshalling by repr happens (search
for `CPointer` handling alongside the call-dispatch code, and whatever emits
"Type check failed for return value").

## Priority note

This single bug blocks the entire `Archive::Libarchive` / `Archive::Libarchive::Raw`
candidate, which is otherwise the strongest archive-format candidate found in
the survey (Artistic-2.0, actively maintained — last push 2025-04-29 — and
covers zip/tar/gzip/bzip2/xz uniformly via libarchive). Fixing this one gap
is likely the highest-leverage single fix for the compression/archive battery
slot.
