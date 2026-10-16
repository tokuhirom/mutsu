use Test;

# `Seq.from-loop`, `Thread.start`, `Thread.is-initial-thread`,
# `X::AdHoc.from-slurpy` and `Supply.start` are rows of the method table,
# found by their owner (ADR-11276 §9.124).

plan 9;

my $i = 0;
is Seq.from-loop({ $i++ }, { $i < 3 }).join(','), '0,1,2', 'Seq.from-loop';
dies-ok { Seq.from_loop({ 1 }) }, 'Seq has no from_loop';

my $t = Thread.start({ 42 });
$t.finish;
isa-ok $t, Thread, 'Thread.start';
ok Thread.is-initial-thread, 'Thread.is-initial-thread in the main thread';
my $in-thread;
Thread.start({ $in-thread = Thread.is-initial-thread }).finish;
nok $in-thread, 'and not in a started thread';

my $e = X::AdHoc.from-slurpy(3, False, 'Not here');
isa-ok $e, X::AdHoc, 'X::AdHoc.from-slurpy';
is $e.message, '3FalseNot here', 'its message joins the parts';

my @starts = Supply.from-list(1, 2).start({ $_ * 10 }).list;
is @starts.elems, 2, 'Supply.start makes one supply per value';
is (await @starts.map(*.Promise)).sort.join(','), '10,20', 'each runs the block';
