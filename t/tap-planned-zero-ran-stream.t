use v6;
use lib $*PROGRAM.parent(2).add("roast/packages/Test-Helpers/lib");
use lib 't/lib';
use Test;
use Test::Util;

# Regression pin (todo/tickets/tap-planned-but-ran-zero-summary-wrong-stream.md):
# rakudo's `Test.rakumod` prints the "You planned N test(s), but ran M"
# end-of-run summary to stdout when NO test ever ran (`ran == 0`), but to
# stderr for the ordinary "ran fewer than planned but at least one" mismatch.
# mutsu printed the zero-run case to stderr too, silently dropping it from any
# program that only inspects stdout (as real `raku` does not).

plan 2;

is_run 'use Test; plan 1; say "done";',
    {
        status => 255,
        out    => "1..1\ndone\n# You planned 1 test, but ran 0\n",
        err    => '',
    },
    'a plan with zero tests run prints the summary on stdout, matching raku';

is_run 'use Test; plan 2; ok True, "one"; say "done";',
    {
        status => 255,
        out    => "1..2\nok 1 - one\ndone\n",
        err    => "# You planned 2 tests, but ran 1\n",
    },
    'a plan with at least one test run still prints the summary on stderr';
