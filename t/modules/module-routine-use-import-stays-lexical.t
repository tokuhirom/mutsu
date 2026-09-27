# A `use` inside a routine body imports lexically into that body only.
#
# An operator imported that way is aliased under the routine's unit package
# (`RoutineUseOpLeak::infix:<**>`), which has the same shape as the module's
# own definitions, so `pop_import_scope` kept it when the routine returned.
# Every later `**` in the package -- including a `where` clause of a sibling
# multi -- then dispatched to the imported candidate. Found through the
# Bitcoin distribution: `secp256k1` does `use FiniteField` inside `Point`'s
# methods, and its `where 1 < $n < 2**256` guard started calling
# FiniteField's modular `infix:<**>`.
use lib 't/lib';
use Test;
use RoutineUseOpLeak;

plan 6;

is AT-LOAD, 'imported', 'the import is visible inside the routine at load time';
is with-import(), 'imported', 'and on a later call';
is plain(), 8, 'a sibling routine does not see the import';
is guarded(1), 'small', 'a sibling multi\'s where clause uses the core operator';
is guarded(100), 'large', 'and still rejects a value it should reject';
is 2 ** 3, 8, 'the importing script does not see it either';
