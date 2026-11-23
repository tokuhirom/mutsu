use Test;

# `Str.substr` with a start outside `0..chars`, or a negative length, returns
# rakudo's Failure of X::OutOfRange, whose message names the argument and
# hints at indexing relative to the end. A negative Int start does not count
# from the end; only a WhateverCode does.

plan 10;

my $r = "abc".substr(10);
nok $r.defined, 'a start past the end is a Failure';
is $r.exception.message, 'Start argument to substr out of range. Is: 10, should be in 0..3; use *-10 if you want to index relative to the end',
    'its message';

$r = "abc".substr(-1);
nok $r.defined, 'a negative Int start is a Failure too';
is $r.exception.message, 'Start argument to substr out of range. Is: -1, should be in 0..3; use *-1 if you want to index relative to the end',
    '...with the same message shape';

$r = "abc".substr(1, -5);
is $r.exception.message, 'Number of characters argument to substr out of range. Is: -5, should be in 0..^Inf; use *-5 if you want to index relative to the end',
    'a negative length';

is "abc".substr(*-1), 'c', 'a WhateverCode start counts from the end';
is "abc".substr(3), '', 'a start at the end is the empty string';
is "abc".substr(1, 0), '', 'a zero length is the empty string';
isa-ok "abc".substr(10).exception, X::OutOfRange, 'the exception is X::OutOfRange';
is "abc".substr(10).exception.comment, 'use *-10 if you want to index relative to the end', '.comment';
