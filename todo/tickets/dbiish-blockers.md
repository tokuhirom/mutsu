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

## Status: mutsu 1/9

Re-measured **2026-07-26** with `tmp/dbiish-survey.sh` (in this repo's `tmp/`,
recreate it from the recipe above), debug build, both interpreters on the same
`-I` line. This is the first survey whose `-I` was actually honoured — see the
section below — so it supersedes everything measured before it.

| File | raku | mutsu | Blocker |
| --- | --- | --- | --- |
| `02-meta` | PASS 1/1 | **PASS 1/1** | — |
| `03-lib-util` | 1 subtest fails | ran 3/5, dies | ② `Unknown function: cannon-name` |
| `44-sqlite-memory` | 1 subtest fails* | ran 0/109, dies | ② `Unknown function: cannon-name` |
| `45-sqlite-common` | 1 subtest fails* | ran 0/109, dies | ② `Unknown function: cannon-name` |
| `46-sqlite-blob` | PASS 18/18 | ran 0/18, dies | ② `Unknown function: cannon-name` |
| `48-sqlite-errors` | PASS 17/17 | ran 2/17, dies | ② `Unknown function: cannon-name` |
| `01-basic` | PASS 35/35 | ran 0/35, dies | ③ `PackageHOW.method_table` |
| `05-mock` | PASS 16/16 | 1 fail of 13 run | ④ `IterationEnd` from a row fetch, then `Too many positionals passed; expected 0 arguments but got 2` |
| `06-types` | PASS 12/12 | 2 fail of 3 run | ⑤ `Int is builtin` / `So not defined`; mutsu suggests `Did you mean 'invert'?` |

\* raku is not clean on `03-lib-util`, `44-` and `45-` either: one subtest each.
Do not chase those — the achievable target is raku parity, not 109/109.

**② is worth five files and is now root-caused** (below). **Nothing fails inside
NativeCall**: the surface `OpenSSL` needs (CStruct, opaque pointers, callbacks)
is strictly harder than SQLite's, and it is holding.

## The first round of these numbers was taken with the wrong `NativeLibs`

`-I` used not to override an installed module of the same name (raku's does), so
every run in the first survey loaded the site repo's `NativeLibs` **0.0.8**
instead of the 0.0.9 the `-I` line pins — a differently-shaped `cannon-name`.
That is fixed
([`news/2026-07/dash-i-beats-installed-modules.md`](../../news/2026-07/dash-i-beats-installed-modules.md));
the tell was a stack frame pointing into
`~/.local/share/mutsu/repo/site/sources/…`, and those frames now name
`../NativeLibs-0.0.9/lib/NativeLibs.rakumod`. The table above is the re-run.

## ① Parse failure — FIXED, was worth four files

`Failed to parse module 'DBIish::CommonTesting': X::Comp::Group: Missing block`.

A class declared inside a `package` block was not a type name to the parser, so
the `when X::DBIish::LibraryMissing { … }` in `CommonTesting`'s `CATCH` could not
parse. Fixed — see
[`news/2026-07/package-nested-class-is-a-parser-type-name.md`](../../news/2026-07/package-nested-class-is-a-parser-type-name.md).
All four affected files now parse and reach their TAP plan; they fail later, on
② below.

## ② `Unknown function: cannon-name` — worth **five** files

All five now fail with the same message. Getting them here took two steps.

**Cleared: the `NativeHelpers::Blob` load.** Four of them used to die earlier,
inside a `CHECK`, because `NativeHelpers::Blob` could not be loaded at all: its
`MoarVM::Guts::REPRs` needs `nativesizeof`, a dereferenceable `Pointer.WHERE`,
positional `Pointer.new` and reads through a `nativecast`ed `CArray` handle —
none of which mutsu had. Those are in now; see
[`news/2026-07/nativecall-sizeof-and-pointer-where.md`](../../news/2026-07/nativecall-sizeof-and-pointer-where.md).
The *rest* of that module — `BODY_OF` / `pointer-to()`, which hand C the address
of a container's element buffer — needs a stable native allocation behind
`Blob`/`array`/`CArray`, i.e. a value-representation change with its own design
work. That half stays in
[`todo/deep/nativehelpers-blob-moarvm-guts.md`](../deep/nativehelpers-blob-moarvm-guts.md);
`DBDish::SQLite` only uses `blob-from-pointer`, which does not go through it.

**Remaining: `cannon-name` itself — ROOT-CAUSED 2026-07-26.** It has nothing to
do with `proto`/`multi`, `sub EXPORT`, or NativeCall. `cannon-name` is only ever
called from *inside* `NativeLibs.rakumod`, at lines 131 and 134, which are in a
method of `class Searcher` — and **a class declared inside a `module` cannot see
that module's subs**:

```raku
unit module NL;
our sub cannon-name($libname) { "cn:$libname" }
class Searcher {
    method try-versions($libname) { cannon-name($libname) }   # Unknown function
}
```

The full variant matrix, the diagnosis (the module-scope *variable* is visible
and the *qualified* call works, so bare-name function lookup is not walking the
enclosing package chain) and the suggested fix are in
[`class-in-module-cannot-see-module-subs.md`](class-in-module-cannot-see-module-subs.md).
**That ticket is the next thing to do here: it is worth five of the nine files.**

The earlier reductions recorded here were not wrong so much as aimed one row off
the failing case — they all call from a sibling *sub*, which works. (They were
also checked while the installed 0.0.8 was being loaded; the matrix in the new
ticket was measured against 0.0.9 with a working `-I`.)

## ③ `Perl6::Metamodel::PackageHOW.method_table` (`01-basic`)

```
No such method 'method_table' for invocant of type 'Perl6::Metamodel::PackageHOW'
```

`01-basic` walks the metamodel to check the driver interface. `method_table` is a
Rakudo MOP method that mutsu's `PackageHOW` does not implement. Not investigated
beyond the message; check what the test actually asks for before implementing the
whole MOP surface.

## ④ `05-mock` — one subtest, then a hard stop

`A row` expects `'a b 1'` and gets `'IterationEnd'`, and the file then dies at
line 32 with `Too many positionals passed; expected 0 arguments but got 2`
(13 of 16 tests run). raku is 16/16. Two separate symptoms, both unexamined —
the `IterationEnd` leak out of a row fetch is the interesting one and smells
like the `Seq`/iterator-exhaustion family.

## ⑤ `06-types` — `Int is builtin` / `So not defined`

Two of the three tests that run fail, and mutsu volunteers `Did you mean
'invert'?`, so a method the test calls is unresolved and being spell-corrected.
raku is 12/12. Not root-caused; start by reading lines 19-21 of the file and
finding which call produces that suggestion.

## ④ Role attribute not seeded — FIXED; `05-mock` has one subtest left

```
P6opaque: no such attribute '$!parent' on type DBDish::ErrorHandling in a DBDish::ErrorHandling
P6opaque: no such attribute '$!last-exception' on type DBDish::ErrorHandling in a DBDish::ErrorHandling
```

`DBIish` instantiates the `DBDish::ErrorHandling` role directly
(`DBDish::ErrorHandling.new(:parent(Nil))`), which puns it to a class, and its
methods read those attributes privately. A punned role kept its attributes only
as mixin markers, so the private read found nothing. Fixed — see
[`news/2026-07/role-pun-private-attribute.md`](../../news/2026-07/role-pun-private-attribute.md).

`48-sqlite-errors` now reaches ② instead. `05-mock` went from running 0 of its
planned 16 tests to running 12 of them, 11 passing, before aborting:

```
not ok 12 - A row      expected: 'a b 1'   got: 'IterationEnd'
Too many positionals passed; expected 0 arguments but got 2
```

Test 12 is `is $iter.pull-one, ['a','b',1]` where `$iter = $sth.allrows.iterator`
— pulling from a hand-obtained iterator over a user-produced `Seq` yields the
`IterationEnd` sentinel instead of the row. The message after it comes from line
32, `is-deeply $sth.row :hash, …` — a method call with an adverb argument
reaching a zero-arity candidate. Two separate general bugs; neither is
root-caused yet, and neither is `DBIish`-specific.

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
