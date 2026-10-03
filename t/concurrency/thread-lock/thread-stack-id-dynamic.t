use v6;
use Test;

# `$*STACK-ID` (rakudo 2022.06+) identifies the running call stack: the
# mainline is 0, and every `start` block or `Thread` body is a stack of its
# own with a fresh id -- even when one pool worker runs both.

plan 6;

is $*STACK-ID, 0, 'the mainline stack is 0';
sub inner() { $*STACK-ID }
is inner(), 0, 'a routine call stays on the same stack';

my @ids = (^4).map({ await start { $*STACK-ID } });
ok @ids.all > 0, 'each start block runs on a non-main stack';
is @ids.unique.elems, 4, 'each start block gets its own id';

my $thread-id;
my $t = Thread.start({ $thread-id = $*STACK-ID });
$t.finish;
ok $thread-id > 0 && $thread-id != @ids.any, 'a Thread body is a new stack too';
is $*STACK-ID, 0, 'the mainline keeps its id';
