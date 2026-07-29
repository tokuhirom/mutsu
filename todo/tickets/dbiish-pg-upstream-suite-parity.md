# DBIish upstream Pg test suite — remaining parity gaps

Measured 2026-07-29 against a live PostgreSQL 16 (docker `mutsu-postgres`,
port 15432, `PGHOST=127.0.0.1 PGPORT=15432 PGUSER=postgres PGPASSWORD=mutsu
PGDATABASE=dbdishtest DBIISH_WRITE_TEST=YES`; create `dbdishtest` first).
**Set the `-I` include list as a shell array** — the ticket's old warning
applies: a scalar `$INCS` string under zsh silently breaks the module path and
produced a bogus first survey in this very session.

Of the 11 upstream Pg files, 6 match raku exactly (30-pg, 34-pg-types,
36-pg-native, 37-pg-datetime, 38-pg-connection-lock, 38-pg-threads). The
basic + extended e2e scripts (`tmp/dbiish-e2e-pg.raku`,
`tmp/dbiish-pg-extra.raku`) are byte-identical to raku. Remaining:

Re-measured 2026-07-29 (late, with the correct include array, after the
rw-out-param/smiley/ret_struct/Buf-of fixes; raku totals 109/46/17/26/9):

- **`36-pg-blob` (12 ok, 5 fail)** — blocked on
  [`module-loaded-sub-with-tail-var.md`](module-loaded-sub-with-tail-var.md):
  `blob-from-pointer` returns its `memcpy`'s `Pointer` instead of the `Buf`.
- **`36-pg-array` (0 run)** — dies in `_to-array` with "Type Array does not
  support associative indexing": `$element.values[0]<array>` — a hash
  subscript on a `Match`'s positional child comes back as a plain `Array`
  somewhere in the `PgArrayGrammar` match tree.
- **`36-pg-enum` (13 of 26)** — dies at test 14 with "Type check failed for
  an element of %; expected  but got Package" (note the EMPTY expected type):
  a typed hash element check against an enum/type-object value.
- **`38-pg-errors` (7 ok, 1 fail)** — one subtest assertion inside "Incorrect
  column" (the first three subtest checks pass; dig out which of the 15
  fails).
- **`35-pg-common` (76 ok, then dumps core)** — after the mid-file `version`
  type-check error was reached it now segfaults outright; get a fresh gdb
  backtrace before theorizing. The earlier note about `PQlibVersion` falling
  through to its `{ * }` stub ("expected uint32 but got Whatever") still
  stands as the first visible symptom.

Once `module-loaded-sub-with-tail-var` is fixed, re-run the full 11-file sweep
before reading anything into individual numbers.
