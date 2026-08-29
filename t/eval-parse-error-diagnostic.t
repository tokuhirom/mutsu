use Test;
use lib $*PROGRAM.parent(2).add("roast/packages/Test-Helpers/lib");
use Test::Util;

plan 1;

is_run 'use Test; eval-lives-ok q[foo<bar], "broken EVAL";',
    {
        :out(/'not ok 1 - broken EVAL'/),
        :err(/'# Error: Unable to parse'/),
        :status({ $_ != 0 }),
    },
    'an EVAL parse failure has Raku-compatible diagnostic prefix';
