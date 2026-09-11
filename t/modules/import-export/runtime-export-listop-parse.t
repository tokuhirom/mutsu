use v6;
use Test;

# A module that exports through a run-time `sub EXPORT` hook carries no
# `is export` traits, so mutsu's static export scan used to learn nothing from
# it. With the name unknown to the parser, the listop call shape
# `first-word <a b c>` was a hard parse error -- `<` was taken as infix
# less-than and the quote-word list was a syntax error -- even though the
# parenthesised call worked fine.
#
# ADR-0087: the scan now approximates such a module's export set with the
# routines it declares in its own unit scope, which is exactly what the
# dominant `UNIT::.grep: { .key.starts-with('&') }` idiom exports.
#
# https://github.com/tokuhirom/mutsu/issues/7881

plan 5;

use lib 't/lib/RuntimeExport';
use RuntimeExportListop;

is first-word(<abcd abce>), 'abcd',
    'the parenthesised call form still works';

is (first-word <abcd abce>), 'abcd',
    'a listop call with a quote-word argument parses';

is (joined <a b c>), 'a-b-c',
    'a second run-time-exported routine parses as a listop too';

# The approximation is parse-time knowledge only. `private-helper` is declared
# in the module's unit scope but withheld by the hook, so it must stay
# unresolvable at run time -- knowing the name is a routine must not make it
# callable.
dies-ok { EVAL 'private-helper("x")' },
    'a unit-scope routine the hook withholds is still not imported';

# The listop reading must not swallow a genuine numeric comparison.
ok 1 < 2, 'infix < still parses as less-than';
