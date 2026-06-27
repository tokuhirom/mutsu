use Test;

# `.rotor` with a batching sublist length of 0 used to loop forever in mutsu:
# a zero count never advances the cursor, so a lone `rotor(0)` (or any spec
# cycle whose net advance is 0) span the loop forever. A zero count is valid in
# Raku in a *multi*-spec rotor — it yields an empty sublist `()` and the other
# specs advance (roast S32-list/rotor.t exercises `(0..5).rotor(0, 1, *)`), so
# zero must NOT be rejected outright. The fix keeps zero-count batches but stops
# the loop once a full spec cycle makes no progress, so the interpreter never
# hangs. Negative counts remain out of range.

plan 12;

# A multi-spec rotor with a zero count yields an empty sublist (roast case).
is (0..5).rotor(0, 1, *).gist, '(() (0) (1 2 3 4 5))',
    'rotor(0, 1, *) yields an empty sublist for the zero count';

# A lone rotor(0) terminates (does not hang) instead of looping forever.
{
    my $r = (1..5).rotor(0);
    pass 'lone rotor(0) terminates without hanging';
    ok $r.elems >= 0, 'lone rotor(0) returns a (finite) result';
}

# A spec cycle whose net advance is 0 (count + gap == 0) also terminates.
{
    (1..5).rotor(2 => -2);
    pass 'rotor(2 => -2) (zero net advance) terminates without hanging';
}

# Negative counts are still rejected as out of range.
throws-like { (1..5).rotor(-2) }, X::OutOfRange,
    message => /'should be in 1..^Inf'/,
    'negative rotor count throws X::OutOfRange';

dies-ok { (1..5).rotor(-3) }, 'another negative rotor count dies';

# Normal rotor still works (no regression).
is (1..6).rotor(2).gist, '((1 2) (3 4) (5 6))', 'rotor(2) works';
is (1..6).rotor(2 => -1).gist, '((1 2) (2 3) (3 4) (4 5) (5 6))',
    'rotor with negative gap works';
is (1..7).rotor(2, 3).gist, '((1 2) (3 4 5) (6 7))', 'rotor with multiple specs works';
is (1..6).rotor(1..*).gist, '((1) (2 3) (4 5 6))', 'rotor with a cycling range count works';
is (1..6).rotor(3, :partial).gist, '((1 2 3) (4 5 6))', 'rotor :partial works';
is (1..6).rotor(2).elems, 3, 'rotor(2) produces 3 batches';
