use Test;

# The handle classes' methods are rows (ADR-11276 §9.157): `Lock`,
# `Lock::Async`, `Lock::ConditionVariable`, `Cancellation`, `Tap`,
# `IO::Socket::Async::ListenSocket` and `Proc`.

plan 24;

my $l = Lock.new;
is $l.protect({ 42 }), 42, 'Lock.protect answers the block';
throws-like { $l.protect(%()) }, X::Multi::NoMatch, 'Lock.protect needs a block';
$l.lock;
$l.unlock;
pass 'Lock.lock / .unlock';
my $c = $l.condition;
isa-ok $c, Lock::ConditionVariable, 'Lock.condition';

my $done = False;
my $t = start {
    $l.protect: { $done = True; $c.signal };
};
$l.protect: { $c.wait({ $done }) };
await $t;
ok $done, 'wait with a test returns once it holds';
$c.signal_all;
pass 'signal_all with no waiter';

my $al = Lock::Async.new;
isa-ok $al.lock, Promise, 'Lock::Async.lock is a Promise';
$al.unlock;
is $al.protect({ 7 }), 7, 'Lock::Async.protect';
dies-ok { $al.condition }, 'Lock::Async has no condition, as in Rakudo';

is $l.protect({ $l.protect({ 'nested' }) }), 'nested', 'a Lock is reentrant';

my $can = $*SCHEDULER.cue({ ; }, :in(100));
isa-ok $can, Cancellation, 'cue :in answers a Cancellation';
nok $can.cancelled, 'not cancelled yet';
$can.cancel;
ok $can.cancelled, 'cancelled after cancel';

my $s = Supplier.new;
my @seen;
my $tap = $s.Supply.tap({ @seen.push: $_ });
$s.emit(1);
ok $tap.close, 'Tap.close';
$s.emit(2);
is-deeply @seen, [1], 'nothing reaches a closed tap';
dies-ok { $tap.cancel }, 'Tap has no cancel, as in Rakudo';

my $listen = IO::Socket::Async.listen('127.0.0.1', 0).tap({ ; });
isa-ok $listen, IO::Socket::Async::ListenSocket, 'a listening tap is a ListenSocket';
isa-ok $listen, Tap, 'which is a Tap';
ok $listen.socket-port.result > 0, 'ListenSocket.socket-port';
$listen.close;

my $p = run $*EXECUTABLE, '-e', 'exit 3';
is $p.exitcode, 3, 'Proc.exitcode';
is +$p, 3, 'Proc.Numeric';
nok ?$p, 'Proc.Bool of a failed child';
my $q = Proc.new;
ok $q.spawn($*EXECUTABLE, '-e', ''), 'Proc.spawn runs a command';
is $q.exitcode, 0, 'and updates the receiver';
