# Battery: database layer — `DBIish` (SQLite)

**Slot:** Database / persistence · **Chosen:** `DBIish`
(`auth<zef:raku-community-modules>`, v0.6.8, BSD-2-Clause) · **Kind:** Adopted
(community module, to be vendored as-is) · **Yardstick:**
[BATTERIES.md §2](../../BATTERIES.md#2-selection-criteria) — license (hard gate)
→ dependency weight → proven behaviour on mutsu → API fit → "a small web blog can
be written with the bundle alone"

Surveyed with the procedure in [selection-method.md](selection-method.md).

## Status: selected, NOT yet bundled

This is a **selection record ahead of the vendoring**, which is deliberate: the
winner is decided and its evidence is written down, but `DBIish` does not run on
mutsu yet, so bundling it would ship a battery that cannot be gated. The blockers
are enumerated below and tracked in `todo/tickets/`; bundling follows once they
are cleared.

The slot matters because the bundle can currently *fetch* (HTTP client + TLS),
*render* (`Template::Mustache`) and *parse* (native JSON) — but it cannot
**store**. A blog needs persistence, and SQLite is the shape that needs no server.

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

All three trees that would be vendored are clear:

| Dist | Declared where | License |
| --- | --- | --- |
| `DBIish` 0.6.8 | `META6.json` + `LICENSE` | BSD-2-Clause |
| `NativeLibs` 0.0.9 | `META6.json` + `LICENSE` | Artistic-2.0 |
| `NativeHelpers::Blob` 0.1.9 | **`LICENSE` only** — `META6.json` has no `license` key | Artistic-2.0 |

`NativeHelpers::Blob` ships the full Artistic-2.0 text with a copyright line
("Copyright (c) 2016 by Salvador Ortiz"), so it is declared *somewhere* and
passes [§4](../../BATTERIES.md#4-license-policy). This is **not** a second
`Encode` situation: `Encode` ships no license statement anywhere, which is why it
is carried provisionally. No new provisional exception is needed here.

## What blocks mutsu today

The good news first: **nothing fails in NativeCall.** The foundation laid for
`OpenSSL` (CStruct, opaque pointers, callbacks — a much harder surface than
SQLite's) is carrying the SQLite bindings. The failures are ordinary interpreter
bugs, and one of them accounts for a third of the file count on its own.

| File | mutsu | First observed failure |
| --- | --- | --- |
| `02-meta.rakutest` | **PASS** | — |
| `44-sqlite-memory` / `45-sqlite-common` / `46-sqlite-blob` | FAIL | `Failed to parse module 'DBIish::CommonTesting': X::Comp::Group: Missing block` |
| `01-basic` | FAIL | `No such method 'method_table' for invocant of type 'Perl6::Metamodel::PackageHOW'` |
| `05-mock` | FAIL | `P6opaque: no such attribute '$!parent' on type DBDish::ErrorHandling` |
| `03-lib-util`, `06-types`, `48-sqlite-errors` | FAIL | first line is only a *warning*; not yet root-caused |

**The single biggest lever is the parse error**, which kills three SQLite files at
once and is root-caused down to a five-line repro: a class declared inside a
`package` block is not visible to the parser as a *type name*, so `when
<that name>` fails to parse. `DBIish` declares every one of its exception types
that way (`package GLOBAL::X::DBIish { class LibraryMissing … }`), and
`CommonTesting` dispatches on them in a `CATCH`. Replacing the two `when`
matchers with built-in types makes the module load. Filed as
`todo/tickets/package-nested-class-not-a-parser-type-name.md`.

The remaining blockers are tracked in `todo/tickets/dbiish-blockers.md`.

`DB::SQLite`'s 0/9 has a different first cause — `Unknown function: cannon-name`,
an `our proto sub` in `NativeLibs`, which both candidates depend on. It is filed
separately (`todo/tickets/nativelibs-our-proto-sub-unknown-function.md`) because
`NativeLibs` is on `DBIish`'s dependency list too, so it has to be fixed either
way.

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

## Next steps before this can be bundled

1. Clear the blockers in `todo/tickets/` (parser first — it is worth 3 files).
2. Re-measure; the gate needs a per-file baseline worth pinning.
3. Vendor `DBIish` + `NativeLibs` + `NativeHelpers::Blob` per
   [BATTERIES.md §3](../../BATTERIES.md#3-vendoring-and-resolution), write the
   re-vendoring recipe into this record, and add the dists to `batteries.lock`.
4. `scripts/battery-testsuite.sh --update`, then close the remaining gaps the
   same way the other batteries were closed.
5. Note the runtime requirement: like `OpenSSL`, this battery needs a **system
   `libsqlite3`** at runtime. That belongs in the bundle index row.
