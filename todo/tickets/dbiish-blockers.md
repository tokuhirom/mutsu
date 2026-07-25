# `DBIish` battery — remaining blockers

The database battery is selected but not yet bundled; the reasoning and the
candidate comparison are in [docs/batteries/database.md](../../docs/batteries/database.md).
This file is the ledger of what stops `DBIish` from running on mutsu. Measured
2026-07-25, `DBIish` 0.6.8, debug build of `main`.

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
| `44-sqlite-memory` | FAIL | ① parse |
| `45-sqlite-common` | FAIL | ① parse |
| `46-sqlite-blob` | FAIL | ① parse |
| `01-basic` | FAIL | ② `PackageHOW.method_table` |
| `05-mock` | FAIL | ③ role attribute `$!parent` |
| `03-lib-util` | FAIL | ④ not root-caused |
| `06-types` | FAIL | ④ not root-caused |
| `48-sqlite-errors` | FAIL | ④ not root-caused |

**Nothing fails inside NativeCall.** The surface `OpenSSL` needed (CStruct,
opaque pointers, callbacks) is strictly harder than SQLite's, and it is holding.

## ① Parse failure — worth three files

`Failed to parse module 'DBIish::CommonTesting': X::Comp::Group: Missing block`.

Root-caused and filed separately:
**`todo/tickets/package-nested-class-not-a-parser-type-name.md`** — a class
declared inside a `package` block is not a type name to the parser, so the
`when X::DBIish::LibraryMissing { … }` in `CommonTesting`'s `CATCH` cannot parse.
Fix that first; it is the highest-yield item here.

## ② `Perl6::Metamodel::PackageHOW.method_table` (`01-basic`)

```
No such method 'method_table' for invocant of type 'Perl6::Metamodel::PackageHOW'
```

`01-basic` walks the metamodel to check the driver interface. `method_table` is a
Rakudo MOP method that mutsu's `PackageHOW` does not implement. Not investigated
beyond the message; check what the test actually asks for before implementing the
whole MOP surface.

## ③ Role attribute not seeded (`05-mock`)

```
P6opaque: no such attribute '$!parent' on type DBDish::ErrorHandling in a DBDish::ErrorHandling
```

`DBDish::ErrorHandling` is a role with a `$!parent` attribute; the attribute is
not present on the composed instance. Related in shape to the attribute-cell work
in `news/2026-07/scalar-attribute-subscript-assignment.md`, but it is a
*composition-time seeding* problem rather than a write-path one.

## ④ Not root-caused (`03-lib-util`, `06-types`, `48-sqlite-errors`)

Their first non-TAP line is only a **warning** —
`Use of uninitialized value of type Any in string context` / `Use of Nil in
string context` — which is emitted by both implementations and is *not* the
diagnosis. This exact trap already cost a session on `Template::Mustache`; get
the real failing assertion before forming a theory.

## When these are cleared

Follow the "Next steps before this can be bundled" list at the end of
`docs/batteries/database.md`: re-measure, vendor the three trees, add them to
`batteries.lock`, and baseline the release gate with
`scripts/battery-testsuite.sh --update`.
