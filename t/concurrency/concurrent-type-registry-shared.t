use Test;

# ADR-17216 slice 2: the type registry (classes, roles, enums, subsets,
# method tables) is one world table shared by every thread. A declaration or
# a MOP change made on any thread is seen by every other once a
# synchronisation point (here an `await`) orders it before the read. Each
# expectation matches `raku`.

plan 5;

# The ADR's repro: a class EVAL'd on a worker is visible on the main thread.
await start { EVAL 'class FromThread { method hi { "hi" } }' };
is (try ::('FromThread').hi) // 'not visible', 'hi',
    "a class EVAL'd on a worker is visible on the main thread";

# A role and a class composing it, both declared on a worker.
await start { EVAL 'role RThread { method r { "r" } }; class CThread does RThread { }' };
is (try ::('CThread').new.r) // 'not visible', 'r',
    'a role and a class composing it declared on a worker are usable on the main thread';

# A method added through the MOP on a worker is dispatched on the main thread.
class Base { method b { "base" } }
await start { Base.^add_method('added', method { "added" }); Base.^compose };
is (try Base.new.added) // 'no method', 'added',
    'a method added on a worker is dispatched on the main thread';

# Sibling workers declaring different classes: none is lost.
await (^4).map: -> $i { start { EVAL "class Sib$i \{ method n \{ $i \} \}" } };
is (^4).map({ ::("Sib$_").n }).join(','), '0,1,2,3',
    'classes declared by sibling workers are all registered';

# A class declared on the main thread after a worker started is seen by it.
my $go = Promise.new;
my $p = start { await $go; (try ::('Late').new.v) // 'not visible' };
EVAL 'class Late { method v { "late" } }';
$go.keep;
is await($p), 'late', 'a worker sees a class declared after it was spawned';

