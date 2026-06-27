use Test;

# `.rotor` with a batching sublist length of 0 used to loop forever in mutsu
# (a Fixed(0) count never advances the cursor → hang). raku throws
# X::OutOfRange ("Batching sublist length is out of range. Is: 0, should be in
# 1..^Inf"). Reject every path that can yield a zero count.

plan 11;

# A bare 0 count throws X::OutOfRange (not a hang).
throws-like { (1..5).rotor(0) }, X::OutOfRange,
    message => /'should be in 1..^Inf'/,
    'rotor(0) throws X::OutOfRange instead of hanging';

# 0 count via a Pair (count => gap).
throws-like { (1..5).rotor(0 => 1) }, X::OutOfRange,
    'rotor(0 => 1) throws X::OutOfRange';

# 0 inside a Range of counts.
throws-like { (1..5).rotor(0..2) }, X::OutOfRange,
    'rotor(0..2) throws X::OutOfRange';

# Negative counts still rejected.
dies-ok { (1..5).rotor(-2) }, 'rotor(-2) dies';

# A 0 in a later multi-spec position is also rejected.
dies-ok { (1..5).rotor(2, 0) }, 'rotor(2, 0) dies';

# Normal rotor still works (no regression).
is (1..6).rotor(2).gist, '((1 2) (3 4) (5 6))', 'rotor(2) works';
is (1..6).rotor(2 => -1).gist, '((1 2) (2 3) (3 4) (4 5) (5 6))',
    'rotor with negative gap works';
is (1..7).rotor(2, 3).gist, '((1 2) (3 4 5) (6 7))', 'rotor with multiple specs works';
is (1..6).rotor(1..*).gist, '((1) (2 3) (4 5 6))', 'rotor with a cycling range count works';
is (1..6).rotor(3, :partial).gist, '((1 2 3) (4 5 6))', 'rotor :partial works';
is (1..6).rotor(2).elems, 3, 'rotor(2) produces 3 batches';
