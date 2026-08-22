# `.^set_ver` / `.^set_auth` / `.^set_api` (Metamodel::Versioning) are unimplemented

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`,
`Type/Metamodel/Versioning.rakudoc:27`).

## Root cause

`Metamodel::Versioning` is a metaclass role providing `.^ver`/`.^auth`/`.^api` (read) and
`.^set_ver`/`.^set_auth`/`.^set_api` (write, typically used once at class-definition time, e.g. in
a `BEGIN` block, to programmatically tag a class with a version/author/API-level). mutsu's
`Perl6::Metamodel::ClassHOW` has no `set_ver`/`set_auth`/`set_api` methods at all — calling any of
them throws `X::Method::NotFound`. (The read-side `.^ver`/`.^auth`/`.^api` were not exercised by
this repro since the write call fails first, but they are presumably equally unimplemented or, at
best, partially implemented via the `is Class(:ver<...>)`-style trait path rather than the runtime
`.^set_*` API — worth checking both directions.)

## Minimal repro

```raku
class Versioned { }
Versioned.^set_ver: v0.0.1;
say Versioned.^ver;
```
- `raku`: `v0.0.1`
- `mutsu`: `No such method 'set_ver' for invocant of type 'Perl6::Metamodel::ClassHOW'`

The doc's original example additionally calls `.^set_auth`/`.^set_api` inside a `BEGIN` block and
reads all three back — same root cause, just exercising the sibling methods too.

## Affected files (starting point)

- `Perl6::Metamodel::ClassHOW` implementation — search for `ClassHOW` and existing `ver`/`auth`/
  `api` metaclass method plumbing (likely already exists in read-only form for `is Class(:ver<>,
  :auth<>, :api<>)`-declared classes) in `src/runtime/class.rs` / metamodel-related modules, and
  add the mutating `set_ver`/`set_auth`/`set_api` counterparts.
