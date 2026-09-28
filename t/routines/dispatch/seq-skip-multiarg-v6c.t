use v6.c;
use Test;

plan 3;

throws-like { (1..12).Seq.skip(2, 3, 4) }, X::Multi::NoMatch,
    '6.c Seq.skip rejects multiple counts';
throws-like { (1..12).List.skip(2, 3) }, X::Multi::NoMatch,
    '6.c List.skip rejects multiple counts';
is-deeply (1..5).Seq.skip(2).List, (3, 4, 5),
    '6.c Seq.skip keeps its one-count behavior';
