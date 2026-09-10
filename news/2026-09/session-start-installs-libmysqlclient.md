# The SessionStart hook installs libmysqlclient

Remote containers ship `libpq` and `libsqlite3` in their base image but not
`libmysqlclient`, so every DBIish MySQL file in the battery suite died with

```
NativeCall: symbol 'mysql_init' not found in 'this process': dlsym failed
```

raised from `DBDish/mysql.rakumod`, scoring `FAIL(ok=0/N,notok=0)` — zero
assertions ran, none failed. Eight files sit in `batteries-whitelist.txt` behind
that library (`20-mysql`, `24-mysql-types`, `24-mysql-types-json`,
`25-mysql-common`, `26-mysql-blob`, `27-mysql-datetime`,
`28-mysql-connection-lock`, `28-mysql-threads`), which meant
`scripts/battery-testsuite.sh` could never come back green in a remote session
and its output had to be read against a documented list of expected
environmental failures.

`.claude/hooks/session-start.sh` now installs it, alongside the rustc and raku
provisioning it already did. The new `setup_native_libs()` walks a `NATIVE_LIBS`
array of `"<soname> <apt package>"` rows, checks each soname against
`ldconfig -p` (comparing the first field exactly — `ldconfig` indents its
listing with a tab, so a substring match on `" $soname "` silently never fires),
and `apt-get install`s only what is missing. A failed first attempt is retried
once after `apt-get update`, since a stale package index is the usual cause and
the update is slow enough to be worth skipping when it is not needed. If the
session is not root, or has no `apt-get`, the hook warns and carries on rather
than failing the session.

The array holds a single row today:

```
libmysqlclient.so.21 libmysqlclient21
```

`default-libmysqlclient-dev` is deliberately *not* used: on Ubuntu it pulls
MariaDB's `libmariadb.so.3`, and DBIish asks
`NativeLibs::Searcher.try-versions('mysqlclient', 16..21)` for a versioned
`libmysqlclient.so.NN`, which that name does not match. Ubuntu 24.04's
`libmysqlclient21` provides exactly `libmysqlclient.so.21`.

Measured on a fresh remote container: the install costs about 2s, and the whole
hook then runs in ~0.14s on every later session where the library is already
present. Both branches were exercised by removing the package and re-running the
hook.

The environmental-failure list in `docs/agent-environments.md` was updated
accordingly: a `dlsym failed` there is no longer "just the container", it means
the hook did not run or could not reach the archive, and the fix is to re-run it
by hand with `MUTSU_SETUP_FORCE=1 .claude/hooks/session-start.sh`.
