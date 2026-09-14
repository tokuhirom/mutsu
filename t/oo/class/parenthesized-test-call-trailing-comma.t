use Test;

plan 2;

is(1, 1, 'the first parenthesized assertion ends before a trailing comma'),
is(2, 2, 'the next assertion remains a separate statement');
