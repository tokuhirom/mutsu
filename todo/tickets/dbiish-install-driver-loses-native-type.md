# A driver loaded through `DBIish.install-driver` cannot see its own native types

`DBIish.connect('mysql', …)` dies with

```
No such method 'mysql_init' for invocant of type 'Str'
```

`MYSQL` is `class MYSQL is export is repr('CPointer')` in
`DBDish::mysql::Native`, `use`d by `DBDish::mysql`, and `DBDish::mysql.connect`
opens with `my $mysql-client = MYSQL.mysql_init;`. The invocant being a **`Str`**
means the bareword `MYSQL` fell through to the bareword-as-string fallback: the
type is not visible from the method body on this load route.

It is specifically the `install-driver` route. Loading the same driver by hand
works end to end (this connects to a live MariaDB and prints `conn: True`):

```raku
my \M = (require ::("DBDish::mysql"));
my $d = M.new(:parent(Any));
$d.connect(:host<127.0.0.1>, :port(13306), :user<root>, :password<mutsu>, :database<testdb>);
```

while going through `DBIish` does not:

```raku
use DBIish;
my $d = DBIish.install-driver('mysql');   # driver: DBDish::mysql resolved=True
$d.connect(:host<127.0.0.1>, :port(13306), :user<root>, :password<mutsu>, :database<testdb>);
# No such method 'mysql_init' for invocant of type 'Str'
```

`install-driver` (`lib/DBIish.rakumod`) wraps the `require` in
`$installed-lock.protect: { %installed{$drivername} //= do { CATCH {…}; … M.new(…) } }`,
so the module — and the transitive `use DBDish::mysql::Native` inside it — is
loaded from within a natively-invoked callback, memoised in a file-scope hash,
and the driver's methods are then called from outside that callback. The
neighbouring memory of `#5379` (a `subtest` rewinding the type registry while
`loaded_modules` was not rewound) makes a registry-lifetime problem the first
hypothesis.

It is not simply "`require` inside `Lock.protect`", nor "`require` inside a `do`
with a `CATCH`", nor "a required class's method reading a type exported by a
nested module it `use`s" — reduced probes of all three pass (see
`tmp/lockreq.raku`, `tmp/lockreq2.raku`, `tmp/lockreq3.raku` against
`tmp/modprobe/lib`). Finding the actual discriminator is the work.

## Repro

```
cd tmp/dbslot/DBIish-0.6.8
../../../target/debug/mutsu -I lib -I ../NativeLibs-0.0.9/lib \
    -I ../NativeHelpers-Blob-*/lib ../../mysqlprobe4.raku
```

(needs the `mutsu-mariadb` container on port 13306; `tmp/mysqlprobe3.raku` is the
working hand-loaded comparison, `tmp/dbiish-e2e.raku` the full script.)

## Impact

Last known blocker on `DBIish`'s real end-to-end mysql path. The previous two —
an `is rw` sub's `Proxy` not being FETCHed on the OTF-compiled call branch, and a
`Proxy` `FETCH` losing its capture to a same-named caller lexical — are fixed
(`news/2026-07/rw-sub-proxy-fetch-on-otf-call.md`,
`news/2026-07/closure-capture-beats-same-named-caller-lexical.md`).
