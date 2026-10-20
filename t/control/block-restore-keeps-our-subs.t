use Test;
use lib 't/lib';

# A block that declares a routine rolls the routine table back on exit, but an
# `our sub` outlives its block. The restore's answer to "which `our` subs come
# back" is memoized while the our-scoped table is untouched (#9170), so a loop
# body asks it once, not once per `our` sub per iteration. These pin that the
# memoized answer is the one the full check gives. The same holds for the
# routines a module load registered: a restore puts back those its map lacks,
# and remembers the last map that lacked none.

plan 8;

our sub top-our { 'top' }

my @got;
for ^3 {
    { sub inner { 'inner' }; @got.push: inner() }
    @got.push: top-our();
}
is @got.join(','), 'inner,top,inner,top,inner,top', 'an our sub stays callable across loop block exits';

for ^2 { { package LoopPkg { our sub lp { 'lp' } } } }
is LoopPkg::lp(), 'lp', 'an our sub declared in a loop block survives';

{ our sub declared-in-block { 'kept' } }
is GLOBAL::declared-in-block(), 'kept', 'an our sub declared in a block is callable after it';

my @shadow;
for ^2 {
    { my sub top-our { 'my' }; @shadow.push: top-our() }
    @shadow.push: top-our();
}
is @shadow.join(','), 'my,top,my,top', 'a my sub shadowing an our sub gives it back on exit';

my $n = 0;
for ^100 { { sub one { 1 }; $n += one() + (top-our() eq 'top' ?? 1 !! 0) } }
is $n, 200, 'many iterations';

for ^2 -> $i {
    { our sub per-iteration { 'p' } }
}
is GLOBAL::per-iteration(), 'p', 'an our sub redeclared per iteration is kept';

package Other {
    our sub other-our { 'other' }
    our sub call-in-loop {
        my @r;
        for ^2 { { sub local-one { 1 } }; @r.push: other-our() }
        @r.join(',')
    }
}
is Other::call-in-loop(), 'other,other', 'inside another package';

my &from-module;
{ use BlockRestoreModuleHelper; &from-module = &outer-call; }
my @mod;
for ^3 { { sub local-two { 2 } }; @mod.push: from-module() }
is @mod.join(','), 'inner!,inner!,inner!',
    'a module first loaded in a block keeps its own routines across later block exits';
