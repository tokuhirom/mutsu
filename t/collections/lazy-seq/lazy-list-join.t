use Test;

plan 9;

is (1, { $_ + 1 } ... *).join(','), '...',
    'join does not pull an endless closure sequence';
is (1..*).join(','), '...',
    'join does not render a capped prefix of an infinite range';

my @a = (1, { $_ + 1 } ... *);
is @a.join(','), '...',
    'join on an array backed by a lazy sequence returns the marker';
is join(',', @a), '...',
    'routine join agrees with method join for a lazy array';

is (1, { last if $_ >= 5; $_ + 1 } ... *).join(','), '...',
    'a self-terminating closure sequence is still lazy before it is pulled';
is (lazy 1, 2).join(','), '...',
    'an explicitly lazy finite list returns the marker';
is (lazy 1, 2).join, '...',
    'the zero-argument join method uses the same lazy marker';
is join(',', 1, (lazy 2, 3)), '1,...',
    'routine join keeps the separator before a lazy argument';
is (1..3).join(','), '1,2,3',
    'an eager finite range still joins its elements';
