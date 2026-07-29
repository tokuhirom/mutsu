# DBIish upstream Pg/mysql test suites — remaining parity gaps

## mysql suite (first measurement 2026-07-29, late)

The 8 mysql files need the server on port **3306** — the tests pass no port
and `MYSQL_TCP_PORT` does not reach libmysqlclient through DBDish. Forward it
(`socat TCP-LISTEN:3306,fork,reuseaddr TCP:127.0.0.1:13306 &`) and create the
fixtures once
(`docker exec mutsu-mariadb mariadb -uroot -pmutsu -e "CREATE DATABASE IF NOT
EXISTS dbdishtest; CREATE USER IF NOT EXISTS 'testuser'@'%' IDENTIFIED BY
'testpass'; GRANT ALL ON dbdishtest.* TO 'testuser'@'%';"`), then
`MYSQL_HOST=127.0.0.1 DBIISH_WRITE_TEST=YES` with the usual `$INC` array.

| file | raku ok/notok | mutsu ok/notok |
| --- | --- | --- |
| `20-mysql` | 89/0 | 88/1 |
| `24-mysql-types` | 5/0 | 1/4 |
| `24-mysql-types-json` | 0 (skip: no JSON::Tiny) | **25/0** (bundled battery — more than raku, fine) |
| `25-mysql-common` | 109/0 | 29/0, then stops |
| `26-mysql-blob` | 10/0 | 8/1 |
| `27-mysql-datetime` | 11/0 | 11/0 ✓ |
| `28-mysql-connection-lock` | 3/0 | 3/0 ✓ |
| `28-mysql-threads` | 1/0 | 1/0 ✓ |

Failure modes not yet dug into (numbers only — start with `25-mysql-common`'s
stop at 29 and `24-mysql-types`' 4 fails).

## Pg suite

Measured 2026-07-29 against a live PostgreSQL 16 (docker `mutsu-postgres`,
port 15432, `PGHOST=127.0.0.1 PGPORT=15432 PGUSER=postgres PGPASSWORD=mutsu
PGDATABASE=dbdishtest DBIISH_WRITE_TEST=YES`; create `dbdishtest` first).
**Set the `-I` include list as a shell array** — the ticket's old warning
applies: a scalar `$INCS` string under zsh silently breaks the module path and
produced a bogus first survey in this very session.

Of the 11 upstream Pg files, 10 match raku exactly (30-pg, 34-pg-types,
35-pg-common, 36-pg-array, 36-pg-blob, 36-pg-native, 37-pg-datetime,
38-pg-connection-lock, 38-pg-errors, 38-pg-threads). The basic + extended
e2e scripts (`tmp/dbiish-e2e-pg.raku`, `tmp/dbiish-pg-extra.raku`) are
byte-identical to raku. Remaining:

Re-measured 2026-07-29 (sixth pass; sweep helper `tmp/pg-sweep.sh`; raku
totals 26):

- `36-pg-blob` — **RESOLVED** (17/17, raku parity) by the six-fix chain in
  `news/2026-07/module-loaded-sub-with-tail-var.md`.
- `36-pg-array` — **RESOLVED** (46/46, raku parity): three quantified-group
  capture-semantics fixes (capturing groups are a capture boundary for inner
  named captures; `Match.values`/`.kv` flatten a quantified `$0`; the
  `for <element>.values` writeback desugar no longer converts a non-Array
  element to an Array), pinned by
  `t/match-quantified-group-capture-semantics.t`.
- `35-pg-common` — **RESOLVED** (109/109, raku parity): the SEGV was a
  `PQclear` double-free — `finish()`'s `with $!result { .PQclear; $_ = Nil }`
  never wrote the Nil back to the attribute cell, so the freed pointer was
  cleared again (`news/2026-07/with-attr-topic-writeback.md`). The old
  "PQlibVersion stub → version type-check" theory was unrelated to the crash.
- **`36-pg-enum` (25 of 26)** — was 13/26 until the Map-metadata empty
  element constraint fix (`news/2026-07/map-defaulted-hash-attr-element-assign.md`).
  The last fail ("Value OK (No eq Yes)") is a closure-capture staleness: a
  converter sub stored via `$dbh.Converter{YesNo} = $yesno` reads the
  captured `$expected` as of store time, missing the mainline's later
  `$expected = 'No'` write. Minimal shape (also trips an "Impossible
  coercion from 'Str' into 'Any'" on the type-object hash key):
  `class K { has %.c; }; my $k = K.new; my $e = "Yes";
  $k.c{Str} = sub ($v) { "$v-$e" }; $e = "No"; say $k.c{Str}("x")`
  — raku warns and prints `x-No`; mutsu dies on the coercion.
- `38-pg-errors` — **RESOLVED** (9/9, raku parity): a handled Failure's
  `.fail` METHOD throws its wrapped exception (the sub form `fail $f`
  re-arms) — `news/2026-07/handled-failure-refail-throws.md`, pinned by
  `t/failure-handled-refail-throws.t`.
