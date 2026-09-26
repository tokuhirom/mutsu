use Test;

# #9504: a closure clone's anonymous `state` lives in the interpreter's store
# keyed by the clone; dead clones' entries are reaped, and every `start` only
# migrates the entries created since the previous spawn. Neither may change
# what a live clone observes.

plan 8;

sub glue { map { ++$ }, 1, 2, 3 }

my $keep = { ++$ };
$keep() for ^3;

# Enough dead clones to trigger several reaping sweeps before any thread.
my $ok = True;
for ^600 { $ok = False unless glue().List eqv (1, 2, 3) }
ok $ok, 'each clone of a map block starts its anonymous state afresh';
is $keep(), 4, 'a surviving clone keeps its state while dead clones are reaped';

is (await start { $keep() }), 5, 'the surviving clone state is migrated into a thread';
is $keep(), 6, '... and the thread write is visible in the parent';

# The same after the shared store is active: new clones get cells there.
$ok = True;
for ^600 { $ok = False unless glue().List eqv (1, 2, 3) }
ok $ok, 'fresh clones after a spawn still start afresh';
is $keep(), 7, 'the surviving clone is unaffected by reaping in shared mode';

# A clone created and advanced between two spawns carries its value over.
my $late = { ++$ };
$late() for ^5;
await start { 1 };
is (await start { $late() }), 6, 'a clone created between spawns is migrated at the next spawn';
is (await (^4).map: { start { 1 } }).elems, 4, 'many spawns with many dead clones still complete';
