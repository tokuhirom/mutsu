# A `sub EXPORT` may return another module's export stash (a Map of its
# symbols). Found via Test::Describe, whose EXPORT re-exports
# `Test::EXPORT::ALL::`.
use Test;
use lib 't/lib';

plan 2;

use ExportStashReexport;

is run-named({ 1 }, 'a'), 'ran [a]', 'a stash returned from EXPORT is imported';
is check(1, 'b'), 'ok b', 'every symbol of it';
