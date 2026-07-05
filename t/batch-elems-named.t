use Test;

# .batch accepts the named form :elems(N) as well as the positional .batch(N).
# mutsu only handled the positional Int, so :elems threw "No such method
# 'batch'".

plan 12;

is (1..10).batch(:elems(4)).raku, "((1, 2, 3, 4), (5, 6, 7, 8), (9, 10)).Seq",
    'Range.batch(:elems(4))';
is [1, 2, 3, 4, 5].batch(:elems(2)).raku, "((1, 2), (3, 4), (5,)).Seq",
    'Array.batch(:elems(2))';
is (1, 2, 3, 4, 5).batch(:elems(2)).raku, "((1, 2), (3, 4), (5,)).Seq",
    'List.batch(:elems(2))';
is (1..10).list.batch(:elems(4)).elems, 3, 'List.batch(:elems) count';
is "abcde".comb.batch(:elems(2)).raku, '(("a", "b"), ("c", "d"), ("e",)).Seq',
    'Seq.batch(:elems(2))';
is [1, 2, 3, 4, 5, 6].batch(:elems(3)).map(*.sum).raku, "(6, 15).Seq",
    ':elems batches feed into map';

# positional form unchanged
is [1, 2, 3, 4, 5].batch(2).raku, "((1, 2), (3, 4), (5,)).Seq", 'positional batch(2)';
is (1..10).batch(3).elems, 4, 'positional batch(3) count';

# :elems and positional agree
is (1..10).batch(:elems(4)).raku, (1..10).batch(4).raku, ':elems == positional';

# zero / negative batch size throws X::OutOfRange
throws-like { [1, 2, 3].batch(:elems(0)) }, X::OutOfRange,
    ':elems(0) throws X::OutOfRange';
throws-like { [1, 2, 3].batch(0) }, X::OutOfRange,
    'positional 0 throws X::OutOfRange';
my $msg;
{ [1, 2, 3].batch(:elems(0)); CATCH { default { $msg = .message } } }
is $msg, "Batching sublist length is out of range. Is: 0, should be in 1..^Inf",
    'error message matches raku';
