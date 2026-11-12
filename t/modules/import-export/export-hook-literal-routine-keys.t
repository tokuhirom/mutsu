use v6;
use lib 't/lib';
use Test;
use LiteralKeyExportHook 'schema';

plan 3;

# A routine a `sub EXPORT` hook names only by a literal `'&name' => ...` key
# (built by a helper the hook calls) is known to the importer's parse, so it
# takes a block argument without parentheses.

is (config { 42 }), 'config(schema): 42', 'listop call with a block argument';
is config({ 43 }), 'config(schema): 43', 'parenthesized call';
is (make-table { 'x' }), 'table: x', 'a hyphenated name';
