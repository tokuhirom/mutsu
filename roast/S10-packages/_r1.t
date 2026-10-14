use Test;
use lib $*PROGRAM.parent(2).add("packages/Test-Helpers");
use Test::Util;
plan *;
{
    class Pub::Bar::Tap { }
    is Pub::Bar::Tap.gist, '(Tap)', 'a';
}
subtest 'x' => {
    plan 1;
    ok 1;
}
