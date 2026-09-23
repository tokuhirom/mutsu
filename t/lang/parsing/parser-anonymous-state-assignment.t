use Test;

plan 2;

is $=1, 1, '$=1 is an ordinary scalar assignment';
is $=(1..3).raku, '1..3', '$=(...) parses as an assignment expression';
