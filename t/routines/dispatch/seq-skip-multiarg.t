use v6.e.PREVIEW;
use Test;

plan 11;

is-deeply (1..12).Seq.skip(2, 3, 4).List,
    (1, 2, 6, 7, 8, 9),
    'Seq.skip alternates produce, skip, and produce counts';
is-deeply (1..10).Seq.skip(2, 3, 1, 2).List,
    (1, 2, 6, 9, 10),
    'an even number of counts produces the remaining tail';
is-deeply (1..10).Seq.skip(2, 3, *).List,
    (1, 2, 6, 7, 8, 9, 10),
    'Whatever in a produce position keeps the remaining tail';
is-deeply (1..10).Seq.skip(0, 3, 4).List,
    (4, 5, 6, 7),
    'zero starts with skipping values';
is-deeply (^20).Seq.skip(|(2, 3) xx *).List,
    (0, 1, 5, 6, 10, 11, 15, 16),
    'an unbounded lazy argument stream uses the same produce/skip pattern';

is-deeply (1..10).List.skip(2, 3, 1).List,
    (1, 2, 6),
    'List.skip uses the same alternating semantics';
is 42.skip(0, 1).List.gist,
    '()',
    'Any.skip treats a scalar as a one-item sequence';

is-deeply (1..5).Seq.skip.List, (2, 3, 4, 5), 'no-argument skip still skips one';
is-deeply (1..5).Seq.skip(3).List, (4, 5), 'one numeric argument is unchanged';
is-deeply (1..5).Seq.skip(*).List, (), 'one Whatever argument is unchanged';
is-deeply (1..5).Seq.skip(*-3).List, (3, 4, 5), 'one Callable argument is unchanged';
