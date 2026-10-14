use Test;
use lib 't/lib';

# A block holding a `use` rolls its imports back on exit, every time it runs
# (#9170: the scope now snapshots the registry tables themselves rather than
# their key sets). Every expectation was checked against rakudo.

plan 6;

my @seen;
for ^3 -> $i {
    use ImportScopeLoopMod;
    @seen.push: isl-plain($i);
    @seen.push: isl-multi($i);
    @seen.push: isl-multi("s$i");
}
is-deeply @seen, [<plain:0 int:0 str:s0 plain:1 int:1 str:s1 plain:2 int:2 str:s2>],
    'the imported routines are visible on every pass of the loop';
nok (try EVAL 'isl-plain(1)'), 'an imported routine is gone after the loop';
nok (try EVAL 'isl-multi(1)'), 'an imported multi is gone after the loop';

sub outer-only($x) { "outer:$x" }
for ^2 { use ImportScopeLoopMod; }
is outer-only(1), 'outer:1', 'a routine declared outside the block survives its import scope';

my $n = 0;
for ^5 { use ImportScopeLoopMod; $n++ }
is $n, 5, 'a block whose import adds nothing new still runs each time';

{
    use ImportScopeLoopMod;
    is isl-multi(7), 'int:7', 'a later block imports the routines again';
}
