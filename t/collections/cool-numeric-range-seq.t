use Test;

plan 20;

# Range and Seq are Cool: a Cool numeric method numifies the receiver to its
# element count (issue #12088).
is (1..3).sin, 0.1411200080598672, 'Range.sin';
is (1..3).log2, 1.5849625007211563, 'Range.log2';
is (1..3).abs, 3, 'Range.abs';
is (1..3).sqrt, 3.sqrt, 'Range.sqrt';
is (1..^5).floor, 4, 'exclusive Range.floor';
is (1..*).abs, Inf, 'endless Range.abs';
is (1..3).sign, 1, 'Range.sign';
is (1..3).round(2), 4, 'Range.round($scale)';
is (1..3).is-prime, True, 'Range.is-prime';
is (1..3).uint8, 3, 'Range.uint8';
is (1..3).int, 3, 'Range.int';
is (1.5..4.5).abs, 4, 'Range with Rat endpoints';
is (1,2,3).Seq.sin, 0.1411200080598672, 'Seq.sin';
is (1,2,3).Seq.sqrt, 3.sqrt, 'Seq.sqrt';
is (1,2,3).Seq.abs, 3, 'Seq.abs';
is (1,2,3).Seq.floor, 3, 'Seq.floor';
is (1,2,3).Seq.uint8, 3, 'Seq.uint8';
is 3.log2, 1.5849625007211563, 'Int.log2 is log(x)/log(2)';
is log2(10), 3.3219280948873626, 'log2 routine form';
dies-ok { (1..3).no-such-method }, 'an unaudited name is still unknown';
