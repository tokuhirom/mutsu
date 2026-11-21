use v6;
use lib 't/lib';
use Test;

# A module that `use`s another module's `infix:<eq>` / `infix:<==>` candidates
# and has a `sub EXPORT` exporting none of them leaves its importer with only
# the core operators -- which autothread a Junction operand. mutsu parsed the
# importer's `eq` as a call of the user operator, found no candidate in scope
# at run time, and fell back to a reduction that compared the Junction whole:
# `"yes" eq any(<y yes>)` was False, so Red's `migration drop-tables`
# confirmation never accepted "yes" (#18478).

plan 10;

use JunctionOpExportSub;

is-deeply ('yes' eq any(<y yes>)).so, True, 'eq autothreads over a Junction';
is ('yes' eq any(<y yes>)).raku, 'any(Bool::False, Bool::True)', 'eq answers a Junction';
is ('a' ne any(<a b>)).raku, 'Bool::False', 'ne is the negation of the threaded eq';
is ('a' lt any(<b a>)).raku, 'any(Bool::True, Bool::False)', 'lt autothreads';
is (1 == any(1, 2)).raku, 'any(Bool::True, Bool::False)', '== autothreads';
is (1 != any(1, 2)).raku, 'Bool::False', '!= is the negation of the threaded ==';
is (1 < any(2, 0)).raku, 'any(Bool::True, Bool::False)', '< autothreads';
ok 'yes' eq 'yes', 'plain strings still compare';
ok 3 == 3.0, 'plain numbers still compare';
is ('a' ~ any(<b c>)).raku, 'any("ab", "ac")', '~ autothreads';
