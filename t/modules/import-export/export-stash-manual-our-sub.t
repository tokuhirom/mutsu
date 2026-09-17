use Test;

# The "manual EXPORT stash" idiom: an `our sub`/`our multi sub` (including a
# custom operator) declared directly inside a module's own
# `my package EXPORT::<tag> { ... }` block is part of that tag's export list
# by construction, even without an explicit `is export` trait on the sub
# itself.
#
# `Net::IP::Parse` (fez, 0.0.7) uses exactly this shape for its
# `infix:<< ip== >>` operator:
#
#   unit module Net::IP::Parse;
#   my package EXPORT::DEFAULT {
#       class IP { ... }
#       our sub infix:<< ip== >> (IP:D $lhs, IP:D $rhs --> Bool:D) { ... }
#   }
#
# Before this fix, `use Net::IP::Parse;` left the operator both unparseable
# (`$a ip== $b` inside the importing file) and, once parseable, unresolved at
# call time ("Unknown function"), because the parser's module-export scan only
# looked for an explicit `is export` trait, and the runtime's export-table
# aliasing (which `import_module` reads by `{module}::{name}`) never ran for
# a sub registered under the literal `EXPORT::DEFAULT::name` key. See #7988.
#
# Every row measured against raku v2026.07; this file passes verbatim there too.

plan 3;

use lib 't/lib';
use ManualExportStashMod;

is greeting("world"), 'hi world',
    'a plain our sub inside EXPORT::DEFAULT is imported and callable';

# The custom infix operator must be BOTH parseable (the operator symbol has to
# be known to the parser scanning the module's exports) and callable (the
# runtime has to resolve it to the routine actually declared).
is (4 stash-eq 4), True,
    'a custom infix operator inside EXPORT::DEFAULT parses and matches';
is (4 stash-eq 5), False,
    'and evaluates correctly for a non-matching pair';
