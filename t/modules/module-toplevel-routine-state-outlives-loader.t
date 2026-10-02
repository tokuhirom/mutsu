use lib 't/lib';
use Test;

# A routine a module declares at its top level is registered once, when the
# module's mainline runs; its `state` belongs to that one registration for the
# life of the program, however the module was reached. A sub declared inside a
# routine is a fresh clone per call of its enclosing routine.
#
# Pins the two invariants that moving a module's top-level registration ids
# out of the loading frame's env (ADR-0084 group 1, #7817) must keep: the id
# outlives the frame that ran the load (here a `require` inside a routine that
# has returned), and a nested routine's registration still mints per call.

plan 5;

sub load() {
    require ToplevelStateTick;
    (&ToplevelStateTick::tick, &ToplevelStateTick::make-counter)
}
my (&tick, &make-counter) = load();

is tick(), 1, 'first call after the loader returned';
is tick(), 2, 'state persists across calls';
is tick(), 3, 'still the same state';

# A sub declared inside a routine is a fresh clone per call of its enclosing
# routine, so its state starts over each time.
my &a = make-counter();
a(); a();
is a(), 3, 'a nested clone keeps its own state';
my &b = make-counter();
is b(), 1, 'a new clone of a nested sub starts afresh';
