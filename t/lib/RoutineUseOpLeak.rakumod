# Fixture for t/modules/routine-use-operator-does-not-leak.t.
unit module RoutineUseOpLeak;

sub with-import is export { use RoutineUseOpLeakOps; 2 ** 3 }

# Called while the module body loads, the way a class's `TWEAK` with a
# nested `use` runs when an `our constant` builds an instance.
our constant AT-LOAD is export = with-import();

sub plain is export { 2 ** 3 }
multi guarded(Int $n where $n < 2 ** 3) is export { "small" }
multi guarded(Int $n) is export { "large" }
