use Test;

# ADR-17216: an enum or subset declared on one thread is a package symbol
# of the one program world, so `::('Name')` finds it on every other thread.
# Expected values checked against raku 2026.09.

plan 10;

await start { EVAL 'subset PosW of Int where * > 0' };
is ::('PosW').^name, 'PosW', 'a subset EVAL\'d on a worker resolves on the main thread';
ok 5 ~~ ::('PosW'), 'its predicate accepts on the main thread';
nok -1 ~~ ::('PosW'), 'its predicate rejects on the main thread';

await start { EVAL 'enum ColorW <RedW GreenW>' };
is ::('GreenW').Int, 1, 'a bare key of an enum EVAL\'d on a worker resolves on the main thread';
is ::('ColorW::GreenW').Int, 1, 'its qualified key resolves too';

my $late = start { sleep 0.2; (try ::('LateKey').Int) // 'not visible' };
EVAL 'enum LateE <EarlyKey LateKey>';
is await($late), 1, 'an enum declared on the main thread after the spawn is seen by the worker';

# The same holds single-threaded once the declaring routine has returned.
sub declare-subset { EVAL 'subset Big of Int where * > 3' }
declare-subset();
is ::('Big').^name, 'Big', 'an our subset outlives the routine that EVAL\'d it';

sub declare-enum { EVAL 'enum Sizes <Small Large>' }
declare-enum();
is ::('Sizes::Large').Int, 1, 'an enum\'s qualified key outlives the routine that EVAL\'d it';

# Lexical declarations stay lexical.
sub my-subset { my subset Hidden of Int where * > 1; 1 }
my-subset();
ok ::('Hidden') ~~ Failure, 'a my subset is not a package symbol';
await start { my enum TE <ta tb>; 1 };
ok ::('tb') ~~ Failure, 'a my enum declared on a worker is not a package symbol';
