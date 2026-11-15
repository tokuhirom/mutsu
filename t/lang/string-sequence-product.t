use Test;

plan 9;

is ('aa' ... 'ba').join(' '), 'aa ba', 'each character position has its own range';
is ('ab' ... 'ca').join(' '), 'ab aa bb ba cb ca',
    'mixed ascending and descending positions form a product';
is ('ac' ... 'bd').join(' '), 'ac ad bc bd', 'the rightmost position changes first';
is ('07' ... '10').join(' '),
    '07 06 05 04 03 02 01 00 17 16 15 14 13 12 11 10',
    'digit strings use character ranges rather than numeric succession';
is ('zz' ... 'aa').head(40).tail, 'ym', 'descending products remain ordered';
is ('aa' ... 'b').join(' '), 'aa', 'an increasing longer seed cannot reach its endpoint';
is ('az' ... 'b').join(' '), 'az', 'a longer seed stops even near the endpoint';
is ('zz' ... 'a').head(40).tail, 'ym',
    'a descending longer seed defers underflow beyond a finite head';
is ('a' ... 'ab').join(' '), 'a', 'a shorter seed keeps its existing endpoint behavior';
