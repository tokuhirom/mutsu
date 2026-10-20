use Test;

# An instance of a built-in class whose state lives in the interpreter (a lock,
# a scheduler, an encoding, a compilation unit, the runtime's result objects)
# has an opaque dispatch shape (ADR-11276 slice 5): its class's rows answer it
# from every call entry -- a direct call, a by-name call, a call on a temporary
# -- and a method its class does not declare is answered as before.

plan 30;

my $lock = Lock.new;
is $lock.protect({ 42 }), 42, 'Lock.protect through the table';
is $lock."protect"({ 7 }), 7, 'Lock.protect by name';
lives-ok { $lock.lock; $lock.unlock }, 'Lock.lock/unlock';
is Lock::Soft.new.protect({ 3 }), 3, 'a Lock::Soft answers Lock rows';
is Lock.new.condition.^name, 'Lock::ConditionVariable', 'Lock.condition on a temporary';

my $la = Lock::Async.new;
lives-ok { await $la.lock; $la.unlock }, 'Lock::Async.lock/unlock';

my $sem = Semaphore.new(1);
$sem.acquire;
nok $sem.try_acquire, 'Semaphore.try_acquire after acquire';
$sem.release;
ok $sem.try_acquire, 'Semaphore.try_acquire after release';

isa-ok $*THREAD.id, Int, 'Thread.id';
ok Thread.is-initial-thread, 'Thread.is-initial-thread on the type object';

my $proc = run $*EXECUTABLE, '-e', 'print 5', :out;
is $proc.out.slurp(:close), '5', 'Proc.out';
is $proc.exitcode, 0, 'Proc.exitcode';
ok ?$proc, 'Proc.Bool';

isa-ok $*SCHEDULER.loads, Int, 'ThreadPoolScheduler.loads';

my $enc = Encoding::Registry.find('utf8');
is $enc.name, 'utf8', 'Encoding::Builtin.name';
is $enc.encoder.encode-chars('é').list, (0xC3, 0xA9), 'Encoding::Encoder::Builtin.encode-chars';
my $dec = $enc.decoder;
$dec.add-bytes('abc'.encode);
is $dec.consume-all-chars, 'abc', 'a decoder consumes its bytes once';
is $dec.consume-available-chars, '', 'and has none left after';

sub f { 1 }
my $handle = &f.wrap(-> { callsame() + 1 });
is f(), 2, 'a wrapped routine';
$handle.restore;
is f(), 1, 'Routine::WrapHandle.restore';

class A { method m { 'A' } }
class B is A { method m { 'B' } }
is B.new.WALK(:name<m>).reverse()().join(','), 'A,B', 'WalkList.reverse';

is (1..4).hyper(:batch(2)).configuration.batch, 2, 'HyperConfiguration.batch';

my $supply = Supply.from-list(1, 2, 3);
is $supply.map(* * 2).list, (2, 4, 6), 'Supply.map through the table';
throws-like { Supply.tap }, X::Parameter::InvalidConcreteness, 'a Supply:D: row refuses the type object';

my $promise = Promise.new;
$promise.keep(5);
is $promise.result, 5, 'Promise.keep/result';
is await(Promise.in(0).then({ 3 })), 3, 'Promise.then';

my $channel = Channel.new;
$channel.send(9);
is $channel.receive, 9, 'Channel.send/receive';
$channel.close;
ok $channel.closed, 'Channel.closed';

my $hyper = (1..6).hyper(:batch(2));
is $hyper.map(* + 1).List, (2..7).List, 'HyperSeq.map';
ok (1..3).race.configuration.batch > 0, 'RaceSeq.configuration';
