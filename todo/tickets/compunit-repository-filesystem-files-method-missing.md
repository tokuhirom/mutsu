# `CompUnit::Repository::FileSystem.files` method is unimplemented

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`,
`Type/CompUnit/Repository/FileSystem.rakudoc:45`).

## Minimal repro

```raku
my $repo = CompUnit::Repository::FileSystem.new(prefix => $*CWD);
say $repo.files('bin/zef', :ver<419.0+>).head.<name> // "Nada";
```

- `raku`: prints `Nada` (no distribution at that prefix advertises a file matching that
  path/version, so `.files` returns an empty list and `.head` is `Nil`, coalescing to
  `"Nada"`).
- `mutsu` (`target/debug/mutsu`): dies with
  `No such method 'files' for invocant of type 'CompUnit::Repository::FileSystem'`.

## Root cause hypothesis

`CompUnit::Repository::FileSystem` (mutsu's module-search-path repository type, used by
`use lib`/`-I`/`MUTSULIB` resolution per the root `CLAUDE.md`) never implements the `.files`
introspection method — it should look up distribution(s) at the repository's prefix matching
the given short-name/version query and return the list of provided files (each with a `name`
key, among others) declared in the matching `META6.json`. Since mutsu's repository is
otherwise functional (module loading itself works), this is purely a missing introspection
method, not a deeper repository-model gap.

## Affected files (starting point)

- Wherever `CompUnit::Repository::FileSystem`'s other methods (`resolve`, `need`, or
  whatever backs `use`/`-I` module resolution) are implemented — likely under
  `src/runtime/` module-loading code — needs a `.files(name, :ver, :auth, :api)` method that
  scans the repository's known distributions' declared `provides`/file list and returns
  matching entries.
