use Test;
use MONKEY-SEE-NO-EVAL;

# #17216: a grammar's rules are methods of the grammar, which is world state.
# A grammar declared on one thread parses on every thread, as on rakudo. Its
# class used to reach other threads, but its rules stayed on the declaring one:
# `G.parse` died with "No such method 'TOP'".

plan 6;

await start { EVAL q[grammar WorkerG { token TOP { <num> }; token num { \d+ } }] };
is EVAL(q[WorkerG.parse("42")<num>.Str]), '42', 'a grammar declared on a worker parses on the main thread';

await start {
    EVAL q[grammar WorkerProto {
        token TOP { <item>+ % ',' }
        proto token item {*}
        token item:sym<num> { \d+ }
        token item:sym<word> { <[a..z]>+ }
    }]
};
is EVAL(q[WorkerProto.parse("1,ab,2")<item>.map(*.Str).join("|")]), '1|ab|2',
    'its proto token candidates reach the main thread too';

await start { EVAL q[grammar WorkerChild is WorkerG { token num { <[a..z]>+ }; }] };
is EVAL(q[WorkerChild.parse("abc")<num>.Str]), 'abc', 'a subclass declared on a worker overrides a rule';
is EVAL(q[WorkerG.parse("7")<num>.Str]), '7', 'the parent grammar keeps its own rule';

# A grammar the main thread declares after a worker started is seen by it.
my $go = Promise.new;
my $later = start { await $go; EVAL q[LateG.parse("x9").Str] };
EVAL q[grammar LateG { token TOP { x \d } }];
$go.keep;
is await($later), 'x9', 'a grammar declared after a spawn parses on the spawned thread';

# A `my token` stays lexical: nothing about it is published.
await start { EVAL q[my token lexical-only { \d }; 1] };
nok (try EVAL q['5' ~~ /<lexical-only>/]), 'a worker-local my token is not visible elsewhere';
