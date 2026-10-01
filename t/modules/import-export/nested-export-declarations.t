use Test;

# The parse-time scans of a module's exports walk every position with the typed
# AST visitor (ADR-0137). rakudo's `is export` exports from any depth of a
# package, so a declaration nested in a routine body, a block or a nested
# package is found too; a lexical or non-exported one there stays private.
# Each expectation below was checked against rakudo.

plan 7;

use lib 't/lib';
use NestedExportDecls;

is (1 nested-cat 2), '1:2', 'an operator exported from inside a routine body parses at its use site';

sub nested-private-const($x) { "mine $x" }
is (nested-private-const 3), 'mine 3', 'a routine-local constant does not shadow an importer routine';

sub nested-private-value($x) { "mine $x" }
is (nested-private-value 4), 'mine 4',
    'a non-exported enum value in a routine body does not shadow an importer routine';

# Two non-multi exports of one symbol anywhere in a package clash.
for (
    'module M1 { sub a is export { 1 }; if True { sub a is export { 2 } } }',
    'module M2 { sub a is export { 1 }; sub f { sub a is export { 2 } } }',
    'module M3 { sub a is export { 1 }; module N { sub a is export { 2 } } }',
    'module M4 { sub a is export { 1 }; class K { sub a is export { 2 } } }',
) -> $code {
    throws-like $code, X::Export::NameClash, "clash: $code";
}
