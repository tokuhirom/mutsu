use Test;
use lib 't/lib';
use DBDishLite;

# Drive a real SQLite database through the DBDishLite *module* — the smallest
# reusable database layer for the web-blog stack. The main program never
# mentions NativeCall or `Pointer`; the module owns all of that. This is the
# end-to-end path that a stale precompilation cache previously masked (the cache
# served a pre-prelude-injection AST and the module hit an undeclared
# `Pointer`); the cache is now keyed on the executable mtime so a rebuilt mutsu
# never reuses a cache produced by older injection logic.
#
# libsqlite3 is not part of the language core, so degrade to a single skip when
# the library cannot be loaded.

unless sqlite-available() {
    plan 1;
    skip 'libsqlite3 not available on this host', 1;
    done-testing;
    exit 0;
}

plan 6;

my $db = connect(':memory:');
ok $db.defined, 'connect(:memory:) returned a connection';

$db.execute('CREATE TABLE posts (id INTEGER, title TEXT, body TEXT)');
$db.execute(q{INSERT INTO posts VALUES
    (1, 'Hello', 'first post'),
    (2, 'World', 'second post'),
    (3, 'Raku',  'third post')});

my @rows = $db.execute('SELECT id, title FROM posts ORDER BY id');
is @rows.elems, 3, 'SELECT returned every row as a hash';
is @rows[0]<title>, 'Hello', 'first row title column';
is @rows[2]<id>, '3', 'third row id column';
is-deeply @rows.map(*<title>).Array, ['Hello', 'World', 'Raku'],
    'rows came back in ORDER BY order with named columns';

my @filtered = $db.execute(q{SELECT title FROM posts WHERE id = 2});
is @filtered[0]<title>, 'World', 'WHERE-filtered query via the module';

$db.close;
