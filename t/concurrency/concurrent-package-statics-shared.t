use Test;

# Package and class-body statics are world state, shared by every thread
# (ADR-17216 slice 4a): a static a worker's `EVAL` declares is visible on the
# main thread, and a write on any thread is seen by every other.

plan 6;

await start { EVAL q[class CW { my $n = 7; method n { $n }; method bump { $n++ } }] };
is ::('CW').n, 7, 'a class-body static declared on a worker is visible on main';
await start { ::('CW').bump };
is ::('CW').n, 8, 'a write on a worker is seen on main';

package Q {
    my @log;
    my $lock = Lock.new;
    our sub add($x) { $lock.protect({ @log.push($x) }) }
    our sub all { @log.sort.join(",") }
}
await (^4).map: -> $i { start { Q::add($i) } };
is Q::all(), '0,1,2,3', 'a package-block array mutated on several threads';

class K {
    my %seen;
    my $lock = Lock.new;
    method see($k) { $lock.protect({ %seen{$k}++ }) }
    method keys { %seen.keys.sort.join(",") }
}
await (^3).map: -> $i { start { K.see("k$i") } };
is K.keys, 'k0,k1,k2', 'a class-body hash mutated on several threads';

await start { EVAL q[package PE { my @items = <a b>; our sub items { @items.join } }] };
await start { EVAL q[class CE { my @xs; method push($x) { @xs.push($x) }; method all { @xs.join(",") } }] };
await start { ::('CE').push(1) };
::('CE').push(2);
is ::('CE').all, '1,2', 'a class-body array declared on a worker, pushed on both threads';
is ::('CW').n, 8, 'the earlier static is unchanged';
