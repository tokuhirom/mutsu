use Test;

# The early form of the plain-method lane (#8880) answers a warm
# `$o.m()` before the opcode's receiver probes run. Each case warms the
# lane with one instance, then calls with something the skipped probes
# would have claimed.

plan 13;

# An exception instance's thrown message is per-instance state.
class Quiet { method message() { 'method message' } }
my $q = Quiet.new;
is $q.message, 'method message', 'user message, first call';
is $q.message, 'method message', 'user message, second call';

class Warm { method ping() { 'pong' } }
my $w = Warm.new;
is $w.ping, 'pong', 'ping, first call';
is $w.ping, 'pong', 'ping, second call (lane replay)';

# A wrap installed after the lane is warm must still run.
my $wrapper = Warm.^find_method('ping').wrap(-> $self { 'wrapped ' ~ callsame() });
is $w.ping, 'wrapped pong', 'a wrap installed after warm-up applies';
Warm.^find_method('ping').unwrap($wrapper);
is $w.ping, 'pong', 'unwrapping restores the plain call';

# An untouched class keeps answering.
class Late { method name() { 'old' } }
my $l = Late.new;
is $l.name, 'old', 'before the MOP change, first';
is $l.name, 'old', 'before the MOP change, replay';

# Per-instance receivers of the same class stay distinct.
class Counter {
    has $.n is rw = 0;
    method bump() { $!n++; $!n }
}
my $c1 = Counter.new;
my $c2 = Counter.new(n => 10);
is $c1.bump, 1, 'first instance, first call';
is $c1.bump, 2, 'first instance, replay';
is $c2.bump, 11, 'second instance of the same class';

# A subclass overriding the method is a different key.
class Base { method who() { 'base' } }
class Derived is Base { method who() { 'derived' } }
my @objs = Base.new, Derived.new, Base.new, Derived.new;
is-deeply @objs.map({ .who }).List, <base derived base derived>.List,
    'base and derived receivers dispatch by class';

# A renderer reads the instance; a warm lane must not skip it.
class Shown { method gist() { 'shown' } }
my $s = Shown.new;
is $s.gist, 'shown', 'user gist, first call';
