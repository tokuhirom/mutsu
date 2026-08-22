# `OUR::` pseudo-package doesn't expose file-scope package variables/routines

Found by the doc-diff harness batch-3 re-run (`docs/doc-diff-backlog.md`,
`Language/syntax.rakudoc:429`).

## Root cause hypothesis

`OUR::` is a pseudo-package giving reflective access to the current package's symbol
table (`OUR::.keys` lists its symbol names; `OUR::name` / `OUR::foo.HOW` access a specific
symbol). mutsu's `OUR::` appears to be backed by a fixed/global symbol list (built-ins,
dynamic variables, core types) rather than the actual current package's stash — it doesn't
include a package-scoped variable declared with `our` in the same file, and looking such a
symbol up by name fails outright.

## Minimal repro

```raku
my $foo::bar = 1;
say OUR::.keys;           # OUTPUT: «(foo)␤»
say OUR::foo.HOW          # OUTPUT: «Perl6::Metamodel::PackageHOW.new␤»
```

- `raku`: `(foo)` then `Perl6::Metamodel::PackageHOW.new`
- `mutsu`: `OUR::.keys` returns a long, unrelated list of ~50 built-in/dynamic-variable
  names (`$*PROGRAM-NAME`, `ThreadPoolScheduler`, `Promise`, ... `foo::bar` appears in
  there as a literal entry, but not `foo` as a package alone), and the second line fails
  outright:
  ```
  Could not find symbol '&foo' in 'OUR'
  ```

## Affected files (starting point)

- Wherever `OUR::` (and likely the sibling pseudo-packages `MY::`, `PROCESS::`, `GLOBAL::`)
  are implemented — probably `src/runtime/` reflection/pseudo-package handling. Needs to
  read from the actual current-package symbol table (built from `our`/package declarations
  in the compiled unit) rather than (or in addition to) a static builtin-name list, and
  needs to resolve `OUR::name` as a package/symbol lookup, not a sub-call lookup (the error
  message `Could not find symbol '&foo' in 'OUR'` suggests it's trying to resolve `foo` as
  a callable `&foo`, when here `foo` is a sub-package created implicitly by
  `my $foo::bar = 1`).
