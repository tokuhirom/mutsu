# Battery: database layer — `DBIish` (SQLite)

**Slot:** Database / persistence · **Chosen:** `DBIish`
(`auth<zef:raku-community-modules>`, v0.6.8, BSD-2-Clause) · **Kind:** Adopted
(community module, to be vendored as-is) · **Yardstick:**
[BATTERIES.md §2](../../BATTERIES.md#2-selection-criteria) — license (hard gate)
→ dependency weight → proven behaviour on mutsu → API fit → "a small web blog can
be written with the bundle alone"

Surveyed with the procedure in [selection-method.md](selection-method.md).

## Status: bundled (partial — SQLite core works, some row-typing gaps remain)

`DBIish` + its two runtime dependencies (`NativeLibs`, `NativeHelpers::Blob`)
are vendored into `modules/` and resolve with **zero config**
(`use DBIish;` — no `-I`, no install). A real SQLite database can be opened,
queried, and written to using nothing but the shipped binary:

```raku
use DBIish;
my $dbh = DBIish.connect('SQLite', database => ':memory:');
$dbh.do('CREATE TABLE t (id INT, name TEXT)');
$dbh.do('INSERT INTO t (id, name) VALUES (?, ?)', 1, 'hello');
say $dbh.execute('SELECT * FROM t').row;   # $["1", "hello"]
```

The slot matters because the bundle can already *fetch* (HTTP client + TLS),
*render* (`Template::Mustache`) and *parse* (native JSON) — but it could not
**store**. A blog needs persistence, and SQLite is the shape that needs no server.

`DBIish` also has full upstream-suite parity with `raku` on PostgreSQL and
MySQL/MariaDB against live servers (see
[`news/2026-07/dbiish-upstream-suite-parity.md`](../../news/2026-07/dbiish-upstream-suite-parity.md)) —
this bundling work is scoped to the generic + SQLite files, since SQLite is the
only engine the "install one binary, no server" premise of the bundle covers;
Pg/mysql/Oracle/SQLCipher still work when a driver library and server happen to
be present, but the release gate (which runs with no such server available)
cannot depend on that — those files gracefully skip via `connect-or-skip`
rather than fail, the same way other batteries' `NETWORK_TESTING`-gated
assertions do (see `docs/batteries/testsuite-gate.md`).

### What still doesn't pass

Two general-purpose mutsu bugs surfaced and were fixed while bundling this
(see the vendoring recipe below for exact numbers):

1. **A native (`is native(...)`) sub called as a bare statement (its return
   value sunk, not assigned) compiled to the `ExecCall` opcode, which never
   checked the NativeCall dispatch table** — only the `CallFunc` opcode did.
   `sqlite3_extended_result_codes($p, 1);` (no assignment) therefore ran its
   literal `{ ... }` stub body instead of the real C call and died with
   "Stub code executed". Fixed generally in `src/vm/vm_call_exec_ops.rs`.
2. **A non-`rw` method whose body is a bare `@!attr`/`%!attr` couldn't be
   indexed-assigned into** (`$sth.column-types[$_] = Rat` raised
   `X::Assignment::RO`), even though Rakudo allows this — an Array/Hash
   attribute is returned by reference, so both whole-value and indexed
   assignment mutate it in place regardless of the accessor's `is rw`-ness;
   only a *scalar* `$!attr` needs `is rw` to expose the container for
   rebinding. Fixed generally in `src/runtime/methods_mut_rw_attr.rs` /
   `methods_mut_method_lvalue.rs`.

Past those two fixes, the remaining gap is a single missing coercion family:
`DBDish::SQLite::StatementHandle::_row` casts an untyped SQLite value to the
caller's declared column type with `$value.$ct` (`$ct` holding a *type object*
like `Int`/`Rat`/`Buf`). mutsu is missing several of these coercion methods —
confirmed missing: `Any.Int`, `Any.Num` (a bare `Any`/`Nil` value has `.Str`
but not `.Int`/`.Num`), `Str.Buf`. Filed as
[`todo/tickets/any-nil-int-num-coercion-missing.md`](../../todo/tickets/any-nil-int-num-coercion-missing.md)
rather than fixed here: it touches the same `Nil`-vs-`Any` representation this
project has already deep-dived once and found no small safe subset of (see
that memory-derived note in the ticket) — it deserves its own dedicated
investigation, not a drive-by fix bundled with the NativeCall/rw-attribute
work above.

### Current per-file status (debug build, generic + SQLite files)

| File | raku | mutsu | Blocker |
| --- | --- | --- | --- |
| `01-basic` | 35/35 | **35/35** | — |
| `02-meta` | 1/1 | **1/1** | — |
| `03-lib-util` | 5/5 | **5/5** | — |
| `05-mock` | 16/16 | **16/16** | — |
| `06-types` | 12/12 | **12/12** | — |
| `44-sqlite-memory` | 109/109* | 52/109 | `Any.Int` coercion missing (above) |
| `45-sqlite-common` | 109/109* | 52/109 | same |
| `46-sqlite-blob` | 18/18 | 8/18 | `Str.Buf` coercion missing |
| `48-sqlite-errors` | 17/17 | 9/17 | not yet root-caused |

\* One subtest in each is `# TODO`-marked upstream ("lack of capabilities
announce") and not a real failure in either implementation.

The five fully-clean files are in `batteries-whitelist.txt` and gate every
release; the four SQLite files with genuine remaining gaps are not — they will
join once the coercion gap above (and whatever `48-sqlite-errors` turns out to
need) is fixed.

## The field

Numbers are whole upstream test files fully passing (a TAP plan, every planned
test `ok`, no `not ok`), run against a plain checkout of the dist with `-I lib`
plus its dependencies. Measured 2026-07-25 against `raku` (Rakudo v2026.06) and a
debug build of mutsu `main`.

Only SQLite is in scope: `libsqlite3` is present on the survey machine while
`libpq` and `libmysqlclient` are not, so the Pg/MySQL/Oracle/SQLCipher files of
both candidates are out of scope for the comparison (they are skipped, not
failed). `DBIISH_WRITE_TEST=YES` was set so the write tests actually run.

| Candidate | Version | Released | License | Runtime deps | Dependents¹ | raku | **mutsu** |
| --- | --- | --- | --- | --- | --- | --- | --- |
| **`DBIish`** | 0.6.8 | 2026-04-12 | BSD-2-Clause | 2 | **449** | **9/9** | **1/9** |
| `DB::SQLite` (+ `DB`) | 0.7 | 2021-04-29 | BSD-2-Clause | 4 | 0 (`DB`: 232) | **9/9** | **0/9** |
| `Red` | 0.2.4 | 2025-11-13 | Artistic-2.0 | 4 | 44 | *not a competitor* | — |
| `Duckie` | 0.0.9 | 2026-01-13 | MIT | 1 | 66 | *wrong engine* | — |
| `Badger` | 1.2.0 | 2024-11-07 | Artistic-2.0 | **0** | 11 | *not a driver* | — |

¹ Distributions in the ecosystem index that declare a dependency on it — computed
over the 2506 distinct dist names in the local REA + fez indices
(`~/.zef/store/{rea,fez}/*.json`), the same data `mzef` uses.

### Why the bottom three are not really in the running

- **`Red` is a consumer, not a competitor.** It is an ORM whose own `depends`
  lists `DBIish` and `DB::Pg`. Bundling it would mean bundling a driver anyway,
  and an ORM is a layer above what this slot is for. It is a good argument
  *for* `DBIish` — the ecosystem's ORM builds on it.
- **`Duckie` is the wrong engine.** DuckDB is a columnar analytics database; the
  blog yardstick wants an embedded transactional store.
- **`Badger` is not a driver.** It exposes SQL files as Raku subs and still needs
  a database layer underneath.

That leaves a straight two-way comparison: **`DBIish` vs `DB::SQLite`**.

## Why `DBIish`

Both are BSD-2-Clause and both are 9/9 under raku, so the license gate and the
upstream-health check do not separate them. The rest does:

- **Ecosystem standing is not close: 449 dependents vs 0.** `DB::SQLite` itself
  has no dependents at all; its `DB` base-roles dist has 232, but that counts the
  whole `DB::*` family. `DBIish` is what the ecosystem actually builds on,
  including `Red`, the ORM most likely to sit on top of this slot later.
- **Maintenance: 2026-04-12 vs 2021-04-29.** `DB::SQLite` has not been released
  in over four years, and its `DB` base is from 2020. `DBIish` is current.
- **Fewer dependencies, and better ones.** `DBIish` needs `NativeHelpers::Blob`
  and `NativeLibs`. `DB::SQLite` needs `BitEnum`, `DB`, `NativeLibs` **and**
  `Concurrent::Stack`, i.e. it drags in a concurrency primitive as well.
- **One driver interface for several engines.** `DBIish` covers SQLite, Pg, MySQL
  and Oracle behind one API, so bundling it now does not foreclose a Postgres
  story later; `DB::SQLite` would need a sibling dist per engine.
- **mutsu is closer to it.** 1/9 vs 0/9 is a small margin, but the *shape* is
  very different — see below.

## Licenses (the hard gate)

All three vendored trees are clear:

| Dist | Declared where | License |
| --- | --- | --- |
| `DBIish` 0.6.8 | `META6.json` (`license` key; no separate `LICENSE` file upstream) | BSD-2-Clause |
| `NativeLibs` 0.0.9 | `META6.json` (`licence` key, British spelling; no separate `LICENSE` file upstream) | Artistic-2.0 |
| `NativeHelpers::Blob` 0.1.9 | **`LICENSE` file** — `META6.json` has no `license`/`licence` key | Artistic-2.0 |

`NativeHelpers::Blob` ships the full Artistic-2.0 text with a copyright line
("Copyright (c) 2016 by Salvador Ortiz"), so it is declared *somewhere* and
passes [§4](../../BATTERIES.md#4-license-policy). This is **not** a second
`Encode` situation: `Encode` ships no license statement anywhere, which is why it
is carried provisionally. No new provisional exception is needed here.

## How mutsu got here

The blockers below were cleared across several sessions; each is a general
interpreter fix, not a DBIish-specific patch (rung 2 of the
[adoption policy](../../BATTERIES.md#1-adoption-policy--community-first-adopt-as-is)).
In order:

1. A **parse error** killed four files at once: a class declared inside a
   `package` block was not visible to the parser as a *type name*, so
   `when <that name>` failed — `DBIish` declares every exception type that way
   (`package GLOBAL::X::DBIish { class LibraryMissing … }`) and
   `CommonTesting` dispatches on them in a `CATCH`. See
   [`news/2026-07/package-nested-class-is-a-parser-type-name.md`](../../news/2026-07/package-nested-class-is-a-parser-type-name.md).
2. **Role punning dropped private attributes**: `DBIish` instantiates the
   `DBDish::ErrorHandling` role directly and its methods read `$!parent` /
   `$!last-exception`. See
   [`news/2026-07/role-pun-private-attribute.md`](../../news/2026-07/role-pun-private-attribute.md).
3. **`NativeHelpers::Blob` couldn't load at all**: its `MoarVM::Guts::REPRs`
   needs `nativesizeof`, a dereferenceable `Pointer.WHERE`, positional
   `Pointer.new`, and reads through a `nativecast`ed `CArray` handle. See
   [`news/2026-07/nativecall-sizeof-and-pointer-where.md`](../../news/2026-07/nativecall-sizeof-and-pointer-where.md).
4. Beyond SQLite, the driver reached **live PostgreSQL and MySQL/MariaDB
   servers** end-to-end — sixteen more general NativeCall/dispatch/parser
   fixes across
   [`dbiish-postgresql-end-to-end.md`](../../news/2026-07/dbiish-postgresql-end-to-end.md),
   [`dbiish-prepared-statements-end-to-end.md`](../../news/2026-07/dbiish-prepared-statements-end-to-end.md)
   and
   [`dbiish-upstream-suite-parity.md`](../../news/2026-07/dbiish-upstream-suite-parity.md)
   (the final nine, closing full raku parity on both drivers).
5. Bundling itself (this record) added the two fixes described above under
   "What still doesn't pass" — the `ExecCall` NativeCall gap and the bare
   Array/Hash rw-attribute indexing gap.

`NativeHelpers::Blob`'s `BODY_OF` / `pointer-to()` — handing C the address of a
container's element buffer — is a separate, *not yet solved* value-representation
question, kept in `todo/deep/nativehelpers-blob-moarvm-guts.md`.
`DBDish::SQLite` does not go through it, so it did not block this bundling.

## Vendoring recipe

```sh
git clone https://github.com/raku-community-modules/DBIish.git /tmp/dbiish-src
cd /tmp/dbiish-src && git checkout 0.6.8   # 34f77e7f1581a67d544f88aa2f10fa90cb3df1b4
cp -r lib "$MUTSU_REPO/modules/DBIish/lib"
cp Changes README.md META6.json "$MUTSU_REPO/modules/DBIish/"
# (dist.ini, CREDITS, examples/, t/ are upstream-only; not vendored)

git clone https://github.com/salortiz/NativeLibs.git /tmp/nativelibs-src
cd /tmp/nativelibs-src && git checkout v0.0.9   # f3e78510702af0eeb49b4418dc59008ff4f8cd1b
cp -r lib "$MUTSU_REPO/modules/NativeLibs/lib"
cp README.md META6.json "$MUTSU_REPO/modules/NativeLibs/"

git clone https://github.com/salortiz/NativeHelpers-Blob.git /tmp/nhb-src
cd /tmp/nhb-src && git checkout 54ac6ddf7f557018a54974692a0a97a2b0aca11b  # matches META6 v0.1.9; untagged
cp -r lib "$MUTSU_REPO/modules/NativeHelpers-Blob/lib"
cp README.md LICENSE META6.json "$MUTSU_REPO/modules/NativeHelpers-Blob/"
```

Then bump the three rows in [`batteries.lock`](../../batteries.lock) and
re-run `scripts/battery-testsuite.sh --update` (release build; needs
`DBIISH_WRITE_TEST=YES`, which the script now exports unconditionally — see
`docs/batteries/testsuite-gate.md`).

**A caveat that was open during the earlier investigation is now resolved:**
`-I` now correctly shadows an installed module of the same name (tracked by
`todo/tickets/dash-i-loses-to-installed-module.md`, since closed) — the numbers
in this record are the real, `-I`-honoring measurements.

## How the field was surveyed

The ecosystem was enumerated from the **local REA + fez indices** rather than by
guesswork: 2506 dists, filtered on name/description/tags for database keywords,
then each candidate's tarball fetched straight from the REA archive at its pinned
version and its own suite run under both `raku` and `target/debug/mutsu`.
Reverse-dependency counts come from the same indices.

Note for whoever reruns this: `raku`'s baseline must be taken **with the
dependency `-I` paths supplied as a shell array**. A `$VAR` holding
`-I a -I b` is passed to the process as a single argument by zsh (no word
splitting), which produces a bogus "everything fails under raku" result.

## Ruled out before measuring

- **`DBIish::Pg` / `MongoDB` / `MySQL`-only dists** — a server-backed database
  contradicts the "install one binary" premise of the bundle. SQLite is the only
  engine that needs nothing else running.
- **`Red`** — see above; an ORM belongs on top of this slot, not in it. Worth
  revisiting as a *later* battery once `DBIish` works, since it is the
  ecosystem's ORM and already targets `DBIish`.

## Runtime requirement

Like `OpenSSL`, this battery needs a **system `libsqlite3`** at runtime — mutsu
does not vendor the C library itself, only the Raku binding.

## Remaining work

1. Fix `todo/tickets/any-nil-int-num-coercion-missing.md` (`Any.Int`/`.Num` and
   friends) — unblocks the rest of `44-sqlite-memory` / `45-sqlite-common`.
2. Root-cause `Str.Buf` (blocks `46-sqlite-blob` past test 8) and whatever
   `48-sqlite-errors` needs (not yet investigated past its first failure).
3. Once those files reach a full, clean TAP pass, add them to
   `batteries-whitelist.txt` via `scripts/battery-testsuite.sh --update` — the
   same way the other five files already gate every release.
4. `Red` (the ecosystem's ORM, built on `DBIish`) remains a candidate for a
   *later* battery on top of this slot.
