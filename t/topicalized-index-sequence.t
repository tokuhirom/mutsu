use Test;

plan 4;

is (.[0] + .[1] given [10, 20]), 30, 'topicalized index access works';

my @a = $[1,2], { $[.[0] + 2, .[1] + 2] } ... *;
isa-ok @a[1], Array, 'sequence keeps itemized array containers from generator';
is @a[1].join('_'), '3_4', 'first generated container has expected values';
is @a[2].join('_'), '5_6', 'second generated container has expected values';
