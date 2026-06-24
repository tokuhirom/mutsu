use NativeCall;

# A tiny DBDish::SQLite-shaped wrapper distributed as a reusable module. It is
# the smallest "real" database layer for the web-blog stack: a `connect` that
# returns a connection object whose `.execute` runs a statement and returns the
# result rows as an array of hashes (column-name => value), plus `.close`.
#
# The whole point of shipping this as a module is that the main program need not
# mention NativeCall or `Pointer` at all — the `Pointer` prelude is injected for
# the module. (A stale precompilation cache used to mask this by serving an old
# post-injection AST; the cache is now keyed on the executable mtime.)
unit module DBDishLite;

sub sqlite3_open(Str, Pointer is rw) returns int32 is native('sqlite3') { * }
sub sqlite3_close(Pointer) returns int32 is native('sqlite3') { * }
sub sqlite3_prepare_v2(Pointer, Str, int32, Pointer is rw, Pointer)
    returns int32 is native('sqlite3') { * }
sub sqlite3_step(Pointer) returns int32 is native('sqlite3') { * }
sub sqlite3_column_count(Pointer) returns int32 is native('sqlite3') { * }
sub sqlite3_column_text(Pointer, int32) returns Str is native('sqlite3') { * }
sub sqlite3_column_name(Pointer, int32) returns Str is native('sqlite3') { * }
sub sqlite3_finalize(Pointer) returns int32 is native('sqlite3') { * }
sub sqlite3_errmsg(Pointer) returns Str is native('sqlite3') { * }
sub sqlite3_libversion() returns Str is native('sqlite3') { * }

constant SQLITE_OK  = 0;
constant SQLITE_ROW = 100;

#| True when libsqlite3 can be loaded on this host.
sub sqlite-available() is export {
    so (try sqlite3_libversion()).defined;
}

class Connection {
    has Pointer $.handle;

    #| Run a statement. For a SELECT, returns an array of row hashes
    #| (column-name => text value). For a non-query, returns the empty list.
    method execute(Str $sql) {
        my $stmt = Pointer.new;
        my $rc = sqlite3_prepare_v2($!handle, $sql, -1, $stmt, Pointer);
        die "prepare failed: " ~ sqlite3_errmsg($!handle) if $rc != SQLITE_OK;
        my @rows;
        my $ncol = sqlite3_column_count($stmt);
        while sqlite3_step($stmt) == SQLITE_ROW {
            my %row;
            for ^$ncol -> $i {
                %row{ sqlite3_column_name($stmt, $i) } = sqlite3_column_text($stmt, $i);
            }
            @rows.push: %row;
        }
        sqlite3_finalize($stmt);
        @rows;
    }

    method close() { sqlite3_close($!handle); }
}

#| Open a connection to the given SQLite database path (e.g. ':memory:').
sub connect(Str $path) is export {
    my $h = Pointer.new;
    die "open failed" if sqlite3_open($path, $h) != SQLITE_OK;
    Connection.new(handle => $h);
}
