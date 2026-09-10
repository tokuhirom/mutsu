use Test;

# End-to-end NativeCall integration: drive a real SQLite database through its C
# API — open, create a table, insert rows, run a prepared SELECT and read the
# result rows, then finalize and close. This exercises the whole NativeCall
# surface together: `is rw Pointer` out-parameters (open / prepare_v2), by-value
# `Pointer` arguments, `int32` and `Str` (`char*`) returns, and the bare
# `Pointer` type object passed as a NULL pointer.
#
# libsqlite3 is preinstalled on common Linux CI images, but it is not part of
# the language core, so the whole file degrades to a single skip when the
# library cannot be loaded — it never fails the suite on a host that lacks it.

use NativeCall;

sub sqlite3_libversion() returns Str is native('sqlite3') { * }
sub sqlite3_open(Str $name, Pointer $db is rw) returns int32 is native('sqlite3') { * }
sub sqlite3_close(Pointer $db) returns int32 is native('sqlite3') { * }
sub sqlite3_exec(Pointer $db, Str $sql, Pointer $cb, Pointer $arg, Pointer $err)
    returns int32 is native('sqlite3') { * }
sub sqlite3_prepare_v2(Pointer $db, Str $sql, int32 $n, Pointer $stmt is rw, Pointer $tail)
    returns int32 is native('sqlite3') { * }
sub sqlite3_step(Pointer $stmt) returns int32 is native('sqlite3') { * }
sub sqlite3_column_int(Pointer $stmt, int32 $col) returns int32 is native('sqlite3') { * }
sub sqlite3_column_text(Pointer $stmt, int32 $col) returns Str is native('sqlite3') { * }
sub sqlite3_finalize(Pointer $stmt) returns int32 is native('sqlite3') { * }
sub sqlite3_errmsg(Pointer $db) returns Str is native('sqlite3') { * }

# Probe whether libsqlite3 is loadable at all; skip the whole file if not.
my $version = try sqlite3_libversion();
without $version {
    plan 1;
    skip 'libsqlite3 not available on this host', 1;
    done-testing;
    exit 0;
}

plan 11;

ok $version ~~ /^ \d+ '.' \d+ /, "sqlite3 libversion looks sane ($version)";

constant SQLITE_OK  = 0;
constant SQLITE_ROW = 100;

my $db = Pointer.new;
is sqlite3_open(':memory:', $db), SQLITE_OK, 'sqlite3_open(:memory:) returns OK';
ok $db.Bool, 'open wrote a non-NULL db handle (is rw out-parameter)';

is sqlite3_exec($db, 'CREATE TABLE t (id INTEGER, name TEXT)', Pointer, Pointer, Pointer),
    SQLITE_OK, 'CREATE TABLE via sqlite3_exec';
is sqlite3_exec($db, q{INSERT INTO t VALUES (1,'alice'),(2,'bob'),(3,'carol')}, Pointer, Pointer, Pointer),
    SQLITE_OK, 'INSERT rows via sqlite3_exec';

my $stmt = Pointer.new;
is sqlite3_prepare_v2($db, 'SELECT id, name FROM t ORDER BY id', -1, $stmt, Pointer),
    SQLITE_OK, 'prepare_v2 a SELECT (is rw stmt out-parameter)';
ok $stmt.Bool, 'prepare_v2 wrote a non-NULL statement handle';

my @rows;
while sqlite3_step($stmt) == SQLITE_ROW {
    @rows.push: sqlite3_column_int($stmt, 0) => sqlite3_column_text($stmt, 1);
}
is @rows.elems, 3, 'stepped over all three rows';
is-deeply @rows, [1 => 'alice', 2 => 'bob', 3 => 'carol'],
    'read back int + text columns for every row';

# A bad statement: exec returns non-zero and errmsg reports it.
isnt sqlite3_exec($db, 'SELECT * FROM nope', Pointer, Pointer, Pointer), SQLITE_OK,
    'a bad statement returns a non-OK code';
like sqlite3_errmsg($db), /'no such table'/, 'errmsg reports the failure';

sqlite3_finalize($stmt);
sqlite3_close($db);
