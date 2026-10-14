use Test;
use lib $*PROGRAM.parent(2).add("packages/Test-Helpers");
use lib $*PROGRAM.parent(2).add("packages/FooBarBaz/lib");
use lib $*PROGRAM.parent(2).add("packages/OverrideTest/lib");
use Test::Util;
plan *;
{ my $x; try { EVAL 'A::B' }; }
{ use Foo; ok 1 }
subtest 'x' => { plan 1; ok 1; }
