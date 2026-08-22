# `CompUnit::Repository::FileSystem`/`Installation` stringify as `.new`, not `inst#<path>`

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Language/perl-var.rakudoc:154`).

## Repro

```raku
.say for $*REPO.repo-chain;
```

- `raku`: prints one line per repo in the chain, each formatted with its short kind
  prefix and path, e.g. `inst#/home/user/.raku`, `ap#`, `nqp#`, `perl5#`.
- `mutsu` (`target/debug/mutsu`): prints the generic default gist, e.g.
  `CompUnit::Repository::FileSystem.new`, `CompUnit::Repository::Installation.new`.

Verified directly:

```
$ raku -e '.say for $*REPO.repo-chain;'
inst#/home/tokuhirom/.raku
inst#/home/.../site
inst#/home/.../vendor
inst#/home/.../core
ap#
nqp#
perl5#
$ target/debug/mutsu -e '.say for $*REPO.repo-chain;'
CompUnit::Repository::FileSystem.new
CompUnit::Repository::Installation.new
```

(The number/kind of repos in the chain will legitimately differ across environments —
that part is not a bug. The bug is the stringification format itself.)

## Root cause hypothesis

`CompUnit::Repository::FileSystem` and `CompUnit::Repository::Installation` (or
whichever native type backs `$*REPO.repo-chain`'s elements) don't define a custom
`.Str`/`.gist` that renders as `<short-name>#<path>` (per
`raku-doc/doc/Type/CompUnit/Repository/FileSystem.rakudoc` and
`raku-doc/doc/Type/CompUnit/Repository/Installation.rakudoc`'s documented `.Str`
behavior), so `.say`/default gist falls back to the generic `TypeName.new` rendering.

## Affected files (starting point)

- Wherever `$*REPO` / `CompUnit::Repository::*` are implemented as builtin types (grep
  for `"CompUnit::Repository"` in `src/runtime/`) — add a `.Str`/`.gist` method.
