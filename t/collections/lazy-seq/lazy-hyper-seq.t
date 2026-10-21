use Test;

# #13918: `.hyper` / `.race` of an unbounded sequence stay lazy, as Rakudo's
# `HyperSeq` batches its source iterator on demand.
plan 8;

is (1..*).map(*+0).hyper.map(*+1).head(3).List, (2, 3, 4), 'hyper over an infinite map pipe, then a stage';
is (1..*).map(*+0).race.map(*+1).head(3).sort.List, (2, 3, 4), 'race over an infinite map pipe, then a stage';
is (1..*).map(*+0).hyper.WHAT.^name, 'HyperSeq', 'a lazy hyper reports HyperSeq';
is (1..*).map(*+0).race.WHAT.^name, 'RaceSeq', 'a lazy race reports RaceSeq';
is (1..*).hyper.map(*+1).head(2).List, (2, 3), 'hyper straight off an unbounded range';
is (1..*).map(*+0).hyper(:batch(4)).grep(* %% 2).head(3).List, (2, 4, 6), 'hyper with :batch, then grep';
is (1..10).map(*+0).hyper.map(*+1).List, (2..11).List, 'a finite sequence still settles';
is (1..5).hyper.WHAT.^name, 'HyperSeq', 'a finite hyper is the settled HyperSeq';
