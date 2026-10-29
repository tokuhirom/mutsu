use Test;

plan 6;

isa-ok (eager race 1..5), List, 'the eager statement prefix turns a RaceSeq into a List';
isa-ok (eager hyper 1..5), List, 'and a HyperSeq';
isa-ok (eager (1..5).race), List, 'eager of a .race call';
is (eager race 1..5), (1, 2, 3, 4, 5), 'the elements are kept';
my $s = (1..3).race;
isa-ok $s.eager, List, '.eager agrees';
isa-ok (race 1..5), RaceSeq, 'race alone stays a RaceSeq';
