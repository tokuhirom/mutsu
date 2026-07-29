# DBIish 0.6.8 — upstream bugs found while validating mutsu

Bugs in DBIish itself (the release we vendor-tested, `DBIish:ver<0.6.8>` from
REA), discovered while bringing mutsu to parity. Each entry reproduces under
**Rakudo**, so none of them is a mutsu compatibility target — mutsu should
match raku's (broken) behavior, and tests must route around them. Recorded
here per the batteries policy (community-first, adopt as-is; upstream fixes go
through PRs against the module, not local patches — see
[BATTERIES.md](../../BATTERIES.md)).

## `Connection.commit` / `Connection.rollback` die: no `protect-connection` on the driver

`DBDish::Connection` (`lib/DBDish/Connection.rakumod`) has:

```raku
method commit {
    ...
    $!parent.protect-connection: {
        $!pg-conn.PQexec("COMMIT");
    }
    ...
}
```

(and the same shape in `rollback`, in the Pg subclass
`lib/DBDish/Pg/Connection.rakumod`). But `$!parent` of a Connection is the
**driver** (`DBDish::Pg`), and `protect-connection` is a method of
`DBDish::Connection` itself — so any call dies:

```
No such method 'protect-connection' for invocant of type 'DBDish::Pg'
```

Verified 2026-07-29 against Rakudo v2026.06 with a live PostgreSQL 16:

```raku
my $dbh = DBIish.connect('Pg', |%connect-args);
$dbh.AutoCommit = False;
$dbh.execute('BEGIN');
$dbh.execute('INSERT ...');
$dbh.rollback;      # ← dies under raku AND mutsu, same message
```

The upstream test suite never calls `commit`/`rollback` through a live
connection (`t/35-pg-common.rakutest` uses SQL-level `BEGIN`/`COMMIT`), which
is why it stays green upstream. **Workaround**: issue `BEGIN` / `COMMIT` /
`ROLLBACK` as SQL through `.execute`, which is what our e2e scripts
(`tmp/dbiish-pg-extra.raku`) do. That SQL-level surface is the raku-parity
target.

Status: not yet reported upstream (candidate for a PR against
`raku-community-modules/DBIish` once the battery is bundled).
