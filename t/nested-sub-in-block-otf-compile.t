use Test;

# A `sub` declared directly inside a bare block/closure body, invoked via
# `.()` (which reaches `call_compiled_closure`), used to be OTF-recompiled --
# or, for a `state`-declaring sub, fully tree-walk-dispatched -- on EVERY
# call instead of resolving through the closure's own cached compiled-fns
# table (news/2026-08/nested-sub-in-block-otf-recompile-fixed.md). The
# perf regression itself is pinned by
# tests/nested_sub_in_block_no_otf_recompile.rs (a MUTSU_VM_STATS check);
# this file pins the plain BEHAVIOR across a handful of shapes.

plan 6;

my $blk = { sub foo () { 42 }; my $r; for ^5 { $r = foo }; $r };
is $blk.(), 42, 'plain nested sub inside a block, called repeatedly via .()';

my $state-blk = { sub foo () { state $x = 0; $x++; $x }; my $r; for ^5 { $r = foo }; $r };
is $state-blk.(), 5, 'state-declaring nested sub inside a block accumulates across calls';

my $reinvoke = { sub bar () { 7 }; bar };
is $reinvoke.(), 7, 'block re-invoked twice sees a stable nested-sub result (1st call)';
is $reinvoke.(), 7, 'block re-invoked twice sees a stable nested-sub result (2nd call)';

# A nested sub with parameters, still called repeatedly inside a loop.
my $addend = { sub add-one($x) { $x + 1 }; my $r; for ^5 { $r = add-one($r // 0) }; $r };
is $addend.(), 5, 'parameterized nested sub inside a block, called repeatedly';

# Two distinct block literals declaring same-named, same-bodied nested subs
# must not collide with each other's compiled-fns entry.
my $a = { sub dup () { "a" }; dup };
my $b = { sub dup () { "b" }; dup };
is (($a.(), $b.()).join(",")), "a,b", 'two blocks with same-named nested subs stay independent';

done-testing;
