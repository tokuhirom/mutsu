use Test;

# The two NativeCall mechanisms DBDish::Pg's `escapeBytea` depends on:
#
#   method PQescapeByteaConn(Buf, size_t, size_t is rw --> Pointer) ...
#   nativecast(Str, $_);
#
# 1. An `is rw` NUMERIC out-parameter of a native METHOD writes back to the
#    caller's variable (the sub form is pinned by
#    t/nativecall-rw-numeric-out-param.t; the method dispatch route resolves
#    the caller variable through the CallMethod op's arg sources instead of a
#    VarRef).
# 2. `nativecast(Str, $ptr)` reads the pointer as a NUL-terminated C string
#    (it used to produce an opaque handle tagged `Str`, stringifying empty).
#
# Driven against libsqlite3 like t/nativecall-sqlite.t: preinstalled on common
# CI images, and the file degrades to a skip when it cannot be loaded.

use NativeCall;

class DB is repr('CPointer') {
    # int sqlite3_db_status(sqlite3*, int op, int *pCur, int *pHiwtr, int reset)
    method sqlite3_db_status(int32 $op, int32 $cur is rw, int32 $hi is rw, int32 $reset --> int32)
        is native('sqlite3') { * }
}

sub sqlite3_libversion() returns Str is native('sqlite3') { * }
sub sqlite3_libversion_ptr(--> Pointer) is symbol('sqlite3_libversion') is native('sqlite3') { * }
sub sqlite3_open(Str $name, Pointer $db is rw) returns int32 is native('sqlite3') { * }
sub sqlite3_close(Pointer $db) returns int32 is native('sqlite3') { * }

my $version = try sqlite3_libversion();
without $version {
    plan 1;
    skip 'libsqlite3 not available on this host', 1;
    done-testing;
    exit 0;
}

plan 5;

constant SQLITE_OK = 0;
constant SQLITE_DBSTATUS_SCHEMA_USED = 2;

# --- nativecast(Str, Pointer) reads a C string ---
my $vp = sqlite3_libversion_ptr();
is nativecast(Str, $vp), $version, 'nativecast(Str, $ptr) reads the NUL-terminated C string';

# --- native METHOD `is rw` numeric out-params write back ---
my $dbp = Pointer.new;
is sqlite3_open(':memory:', $dbp), SQLITE_OK, 'sqlite3_open(:memory:) returns OK';
my $db = nativecast(DB, $dbp);

my int32 $cur = -1;
my int32 $hi = -1;
is $db.sqlite3_db_status(SQLITE_DBSTATUS_SCHEMA_USED, $cur, $hi, 0), SQLITE_OK,
    'sqlite3_db_status via native method returns OK';
ok $cur >= 0, "method is-rw out-param 1 was written ($cur)";
ok $hi >= 0, "method is-rw out-param 2 was written ($hi)";

sqlite3_close($dbp);
