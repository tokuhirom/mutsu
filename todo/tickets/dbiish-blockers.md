# `DBIish` battery — remaining blockers

The database battery is selected but not yet bundled; the reasoning and the
candidate comparison are in [docs/batteries/database.md](../../docs/batteries/database.md).
This file is the ledger of what stops `DBIish` from running on mutsu. Measured
2026-07-25, `DBIish` 0.6.8, debug build of `main`, and re-measured the same day
after the parse blocker was fixed.

Only the generic and SQLite files are in scope — `libpq` / `libmysqlclient` are
not installed on the survey machine, so the Pg/MySQL/Oracle/SQLCipher files are
neither passing nor failing.

## Reproducing

```sh
mkdir -p tmp/dbslot && cd tmp/dbslot
for u in \
 'https://raw.githubusercontent.com/raku/REA/main/archive/D/DBIish/DBIish%3Aver%3C0.6.8%3E%3Aauth%3Czef%3Araku-community-modules%3E%3Aapi%3C1%3E.tar.gz' \
 'https://raw.githubusercontent.com/raku/REA/main/archive/N/NativeHelpers%3A%3ABlob/NativeHelpers%3A%3ABlob%3Aver%3C0.1.9%3E%3Aauth%3Cgithub%3Asalortiz%3E.tar.gz' \
 'https://raw.githubusercontent.com/raku/REA/main/archive/N/NativeLibs/NativeLibs%3Aver%3C0.0.9%3E%3Aauth%3Czef%3Araku-community-modules%3E.tar.gz' ; do
 curl -sSL "$u" | tar xz; done
cd DBIish-0.6.8
INC=(-I lib -I ../NativeLibs-0.0.9/lib -I ../NativeHelpers-Blob-*/lib)
export DBIISH_WRITE_TEST=YES        # required, or the write tests all skip
raku $INC t/45-sqlite-common.rakutest      # baseline: 9/9 files pass
mutsu $INC t/45-sqlite-common.rakutest
```

**`$INC` must be a shell array.** zsh does not word-split a plain scalar, so
`raku $INC …` passes one giant argument and every file "fails" under raku too —
a bogus baseline that wastes a session.

## Status: mutsu 1/9, raku 9/9

| File | mutsu | Blocker |
| --- | --- | --- |
| `02-meta.rakutest` | **PASS** | — |
| `44-sqlite-memory` | FAIL | ② NativeLibs `install-driver` |
| `45-sqlite-common` | FAIL | ② NativeLibs `install-driver` |
| `46-sqlite-blob` | FAIL | ② NativeLibs `install-driver` |
| `03-lib-util` | FAIL | ② NativeLibs `cannon-name` |
| `01-basic` | FAIL | ③ `PackageHOW.method_table` |
| `05-mock` | FAIL | ④ role attribute `$!parent` |
| `48-sqlite-errors` | FAIL | ④ role attribute `$!last-exception` |
| `06-types` | FAIL | ⑤ not root-caused |

**Nothing fails inside NativeCall.** The surface `OpenSSL` needed (CStruct,
opaque pointers, callbacks) is strictly harder than SQLite's, and it is holding.

## ① Parse failure — FIXED, was worth four files

`Failed to parse module 'DBIish::CommonTesting': X::Comp::Group: Missing block`.

A class declared inside a `package` block was not a type name to the parser, so
the `when X::DBIish::LibraryMissing { … }` in `CommonTesting`'s `CATCH` could not
parse. Fixed — see
[`news/2026-07/package-nested-class-is-a-parser-type-name.md`](../../news/2026-07/package-nested-class-is-a-parser-type-name.md).
All four affected files now parse and reach their TAP plan; they fail later, on
② below.

## ② NativeLibs (`03-lib-util`, and the three SQLite files)

`03-lib-util` fails with `Unknown function: cannon-name`; the SQLite files fail
one layer up, inside `NativeLibs::install-driver`, with `An exception occurred
while evaluating a CHECK`. Both point at the same module, so treat them as one
item — it is now the highest-yield blocker here, worth four files.

Filed separately as
`todo/tickets/nativelibs-our-proto-sub-unknown-function.md`. A plain `our proto
sub` module works, so the suspect is the custom `sub EXPORT(|)` that
`NativeLibs.rakumod` declares **before** its `unit module` line (it reaches into
`&trait_mod:<is>.candidates`). Reduce that first.

## ③ `Perl6::Metamodel::PackageHOW.method_table` (`01-basic`)

```
No such method 'method_table' for invocant of type 'Perl6::Metamodel::PackageHOW'
```

`01-basic` walks the metamodel to check the driver interface. `method_table` is a
Rakudo MOP method that mutsu's `PackageHOW` does not implement. Not investigated
beyond the message; check what the test actually asks for before implementing the
whole MOP surface.

## ④ Role attribute not seeded (`05-mock`, `48-sqlite-errors`)

```
P6opaque: no such attribute '$!parent' on type DBDish::ErrorHandling in a DBDish::ErrorHandling
P6opaque: no such attribute '$!last-exception' on type DBDish::ErrorHandling in a DBDish::ErrorHandling
```

`DBDish::ErrorHandling` is a role with `$!parent` and `$!last-exception`
attributes; neither is present on the composed instance. Related in shape to the
attribute-cell work in `news/2026-07/scalar-attribute-subscript-assignment.md`,
but it is a *composition-time seeding* problem rather than a write-path one.
`48-sqlite-errors` was previously filed as not-root-caused; once the parse
blocker was gone it turned out to be this same attribute, so the two files share
one fix.

## ⑤ Not root-caused (`06-types`)

Its first non-TAP line is only a **warning** — `Use of uninitialized value of
type Str in string context`, in the test file's own `BUILD` — which is emitted by
both implementations and is *not* the diagnosis. This exact trap already cost a
session on `Template::Mustache`; get the real failing assertion before forming a
theory.

## When these are cleared

Follow the "Next steps before this can be bundled" list at the end of
`docs/batteries/database.md`: re-measure, vendor the three trees, add them to
`batteries.lock`, and baseline the release gate with
`scripts/battery-testsuite.sh --update`.
