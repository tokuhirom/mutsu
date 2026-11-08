use Test;

plan 8;

# `await` consumes the Awaitable::Handle protocol (ADR-15519): an already
# settled handle, a failed one and a pending one settled by its subscriber.
class Settled does Awaitable::Handle { method subscribe-awaiter(&s) { s(True, 7) } }
class Later does Awaitable::Handle {
    method subscribe-awaiter(&notify) { notify(True, 'late') }
}
class Threaded does Awaitable::Handle {
    method subscribe-awaiter(&s) { start { sleep 0.01; s(True, 'from thread') }; Nil }
}
class Source does Awaitable {
    has $.handle;
    method get-await-handle { $!handle }
}

is await(Source.new(handle => Settled.already-success(42))), 42,
    'an already-successful handle yields its result';

try await Source.new(handle => Settled.already-failure(X::AdHoc.new(payload => 'boom')));
isa-ok $!, X::Await::Died, 'an already-failed handle raises X::Await::Died';
is $!.message, 'boom', 'the failure carries the handle\'s cause';

is await(Source.new(handle => Settled.new)), 7, 'a pending handle takes its result from the subscriber';
is await(Source.new(handle => Later.new)), 'late', 'a subscriber that settles at once is honoured';
is await(Source.new(handle => Threaded.new)), 'from thread', 'a subscriber settled from another thread';

my @r = await Source.new(handle => Settled.already-success(1)), Source.new(handle => Settled.already-success(2));
is-deeply @r, [1, 2], 'a list of awaitables is awaited in order';

is await(start { 5 }), 5, 'a native Promise still awaits';
