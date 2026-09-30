use Test;
use nqp;

# An `is IterationBuffer` subclass keeps its elements in a reserved array
# attribute that nqp code writes into (`nqp::push(self, ...)`). `nqp::create`
# seeded it, but `.new` and `.bless` did not, so the first `nqp::push` on such
# an object died with "got Any" (#10351). Expectations are rakudo's.

plan 11;

my class VL is IterationBuffer is repr('VMArray') {
    method STORE(VL:D: \s, :$INITIALIZE) { for s.list { nqp::push(self, $_) }; self }
    method seen { nqp::elems(self) }
}

# --- the issue's repro: a container declared `is VL` ---
my @a is VL = ^3;
is @a.elems, 3, 'my @a is VL = ^3 stores through nqp::push';
is @a.seen, 3, '... and the storage holds all three elements';

# --- every construction path allocates the storage ---
my \viaNew = VL.new;
nqp::push(viaNew, 5);
is nqp::elems(viaNew), 1, '.new allocates usable storage';

my \viaBless = VL.bless;
nqp::push(viaBless, 9);
is nqp::elems(viaBless), 1, '.bless allocates usable storage';

my \viaCreate = nqp::create(VL);
nqp::push(viaCreate, 7);
is nqp::elems(viaCreate), 1, 'nqp::create still allocates usable storage';

my \base = IterationBuffer.new;
nqp::push(base, 1);
is nqp::elems(base), 1, 'the base type is unchanged';

# --- every object owns its storage ---
my \first = VL.new;
my \second = VL.new;
nqp::push(first, 1);
nqp::push(first, 2);
is nqp::elems(first), 2, 'the first object holds what was pushed onto it';
is nqp::elems(second), 0, 'a second .new starts empty, not sharing the first one\'s storage';

# --- the storage is the object's own elements ---
my \filled = VL.new;
nqp::push(filled, 'x');
nqp::push(filled, 'y');
is nqp::atpos(filled, 0), 'x', 'the first element reads back';
is nqp::atpos(filled, 1), 'y', 'the second element reads back';
is filled.seen, 2, 'a method on the subclass sees the same storage';
