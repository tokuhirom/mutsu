use v6;
use Test;

plan 3;

my sub dies-in-gather() { gather { die 'boom' } }
dies-ok { dies-in-gather().sink },
    'an explicit sink method forces a gather body';

my sub returns-in-gather() { gather { return } }
throws-like { returns-in-gather().sink }, X::ControlFlow::Return,
    'an explicit sink method delivers a dead return from a gather body';

my $gather = gather { take 1 };
$gather.sink;
throws-like { $gather.is-lazy }, X::Seq::Consumed,
    'sinking a gather still consumes the original Seq';
