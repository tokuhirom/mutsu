use Test;

# A value-dependent `multi` (a `where` clause, a subset) used to be resolved
# three times per call, and `push_multi_dispatch_frame` was one of the three:
# it re-resolved the name to work out which candidate was being called, so it
# could exclude that one from the `remaining` list `nextsame`/`callsame` walk.
# It is now handed the winner `compile_and_call_function_def` already holds
# (#7886), so this file pins that the redispatch chain still sees the same
# candidate list. Every expectation below was checked against rakudo first.
#
# The resolution COUNT itself is pinned by tests/multi_call_resolves_once.rs
# (a MUTSU_VM_STATS check); this file pins the behaviour.

plan 8;

{
    my @log;
    multi sub f(Int:D $x where { True }) { @log.push('int'); nextsame; 'int' }
    multi sub f($x) { @log.push('any'); 'any' }
    is f(1), 'any', 'nextsame from a where-constrained winner reaches the wider candidate';
    is @log.join(','), 'int,any', 'both candidate bodies ran, narrowest first';
}

{
    multi sub g(Int:D $x where * > 0) { 'pos:' ~ callsame() }
    multi sub g($x) { 'any' }
    is g(5), 'pos:any', 'callsame from a where-constrained winner returns the wider result';
    is g(-5), 'any', 'a failing where constraint still falls through to the wider candidate';
}

{
    subset Pos of Int where * > 0;
    multi sub h(Pos $x) { 'pos' }
    multi sub h($x) { 'any' }
    is h(3), 'pos', 'a subset-constrained candidate wins for a matching value';
    is h(-3), 'any', 'a subset-constrained candidate is skipped for a non-matching value';
}

{
    # A user-defined operator reaches the same dispatch frame through
    # compile_and_call_function_def, with the winner already resolved.
    multi sub infix:<qq>(Int $a where * > 0, Int $b) { 'pos' }
    multi sub infix:<qq>($a, $b) { 'gen' }
    is (1 qq 2), 'pos', 'a where-constrained operator candidate wins';
    is (-1 qq 2), 'gen', 'a where-constrained operator candidate yields to the generic one';
}

done-testing;
