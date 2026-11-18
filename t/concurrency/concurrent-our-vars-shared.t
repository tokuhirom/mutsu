use Test;

# ADR-17216 slice 4b: `our` variables and `our`-scoped routines are package
# state, one per process. An `our` declared, assigned, mutated or rebound on
# one thread, and an `our sub` declared at run time on one thread, are seen by
# every other once a synchronisation point (here an `await`) orders the write
# before the read. Each expectation matches `raku`.

plan 12;

# An `our` declared by an EVAL on a worker.
await start { EVAL q[our $NEWV = 42;] };
is (try $GLOBAL::NEWV) // 'none', 42, 'an `our` EVAL\'d on a worker is visible on the main thread';

# A package's `our` scalar, array and hash created on a worker.
await start { EVAL q[package PV { our $v = 7; our @a = 1, 2; our %h = a => 1 }] };
is (try $PV::v) // 'none', 7, 'a package `our $` created on a worker';
is-deeply (try @PV::a) // 'none', [1, 2], 'a package `our @` created on a worker';
is-deeply (try %PV::h) // 'none', {a => 1}, 'a package `our %` created on a worker';

# Sibling workers mutating one package's containers through its own sub.
package Q { our @list; our %map; our $count = 0;
            our sub add($x) { @list.push($x); %map{$x} = 1; $count++ } }
my $lock = Lock.new;
await (^4).map: -> $i { start { $lock.protect: { Q::add($i) } } };
is @Q::list.sort.join(','), '0,1,2,3', 'pushes made on workers land in the package array';
is %Q::map.keys.sort.join(','), '0,1,2,3', 'keys set on workers land in the package hash';

# A qualified write of a name the package never declared.
await start { $Q::fresh = 'made on worker' };
is $Q::fresh, 'made on worker', 'a package variable first written on a worker';

# A rebind through the stash, on a worker and in a sub.
package RB { our @list = 1, 2; our $s = 1 }
sub rebind { RB::<@list> := [7]; RB::<$s> := 5 }
rebind();
is-deeply @RB::list, [7], 'an `our @` rebound in a sub is seen by the caller';
await start { RB::<@list> := [8]; RB::<$s> := 6 };
is-deeply @RB::list, [8], 'an `our @` rebound on a worker';
is $RB::s, 6, 'an `our $` rebound on a worker';

# `our sub`s declared at run time on a worker.
await start { EVAL q[package PW { our sub get { "pw" } }] };
is (try &::("PW::get")()) // 'no sub', 'pw', 'an `our sub` EVAL\'d on a worker';
await start { EVAL q[our sub top-our { "top" }] };
is (try &::("top-our")()) // 'no sub', 'top', 'a GLOBAL `our sub` EVAL\'d on a worker';
