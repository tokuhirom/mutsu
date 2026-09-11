use v6;
use Test;

# `eval_block_value_inner` snapshots the routine registry (`functions`,
# `proto_functions`, `proto_subs`) on entry and restores it on exit, so a
# routine declared inside a carrier block -- a `where` clause, a regex code
# block, a `do` block -- is lexical to it.
#
# Those three tables are copy-on-write `Arc`s (#7887), which makes the snapshot
# a refcount bump instead of an O(registered routines) copy that the
# declaration-free block then throws away. The copy moved onto the first write
# inside the block, so what needs pinning is that a write through the shared
# table still leaves the snapshot untouched: a block that declares a routine
# must neither leak it outwards nor lose the outer routine of the same name.

plan 10;

sub f() { 'outer' }

# --- a bare block's `sub` shadows and then gives the name back
{
    sub f() { 'inner' }
    is f(), 'inner', 'a block-scoped sub shadows the outer one inside the block';
}
is f(), 'outer', 'and the outer sub is back after the block';

# --- the same, from a `do` block used for its value
my $inner = do {
    sub f() { 'do-block' }
    f();
};
is $inner, 'do-block', 'a do-block sub shadows inside the block';
is f(), 'outer', 'and the outer sub survives the do-block';

# --- a `where` clause body is a carrier block: its `my sub` stays inside it
sub g($x where { my sub helper() { 7 }; helper() == 7 }) { "ok $x" }
is g(1), 'ok 1', 'a where clause may declare its own helper sub';
is g(2), 'ok 2', 'and the declaration still works on a second call';
nok (try EVAL 'helper()').defined, 'the where clause helper does not leak out';

# --- a regex code block is a carrier block too
ok 'abc' ~~ / <?{ my sub twice($n) { $n * 2 }; twice(2) == 4 }> abc /,
    'a regex code assertion may declare its own sub';
nok (try EVAL 'twice(1)').defined, 'the regex code block sub does not leak out';

# --- a block-scoped `proto`/`multi` group is restored too (proto_subs)
{
    proto sub pp(|) { * }
    multi sub pp(Int $x) { "int $x" }
    is pp(3), 'int 3', 'a block-scoped proto/multi group dispatches inside the block';
}
