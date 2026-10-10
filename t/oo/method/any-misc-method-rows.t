use Test;
plan 17;

# Any.serial, Any.batch and List.chrs are rows (ADR-11276 §9.64); expectations
# checked against raku.
is (1, 2).serial.raku, '(1, 2)', 'List.serial';
is {a => 1}.serial.raku, '{:a(1)}', 'Hash.serial';
my $h = {a => 1};
is $h.serial.raku, '{:a(1)}', 'an itemized Hash loses its marker';
is 5.serial, 5, 'Int.serial';
is "ab".serial, "ab", 'Str.serial';

is (1..7).batch(3).raku, '((1, 2, 3), (4, 5, 6), (7,)).Seq', 'Range.batch';
is <a b c d e>.batch(2).raku, '(("a", "b"), ("c", "d"), ("e",)).Seq', 'List.batch';
my @a = 1..5;
is @a.batch(2).raku, '((1, 2), (3, 4), (5,)).Seq', 'Array.batch is lazy over a cursor';
is @a.batch(:elems(4)).raku, '((1, 2, 3, 4), (5,)).Seq', 'named :elems';
is 5.batch(2).raku, '((5,),).Seq', 'Int.batch';
is Buf.new(1, 2, 3).batch(2).raku, '((1, 2), (3,)).Seq', 'Buf.batch reads the bytes';
throws-like { (1, 2).batch(0) }, X::OutOfRange, 'batch(0) is out of range';
my $m;
try { (1, 2).batch(0); CATCH { default { $m = .message } } }
is $m, 'Batching sublist length is out of range. Is: 0, should be in 1..^Inf', 'message';

is (72, 105).chrs, 'Hi', 'List.chrs';
is [72, 105].chrs, 'Hi', 'Array.chrs';
is (65..67).chrs, 'ABC', 'Range.chrs';
is (65, 66).Seq.chrs, 'AB', 'Seq.chrs';
