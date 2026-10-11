use Test;

# #11701: two routes that structurally touched a shared `has` container with
# nothing held while other threads wrote it.
#
# - `@!a.push(...)` reached `push_to_shared_var`'s in-place fallback, which
#   extended the node every thread reaches through the shared instance with no
#   lock.
# - `%!h.kv` promoted the hash's slots to element cells (walking the map) while
#   another thread's `%!h{$k}++` rehashed it: `capacity overflow` panic.
#
# Both now take the ADR-0068 structure guard on the container node. This pins
# mutsu's memory-safety guarantee, not a Raku one (rakudo itself does not make
# unlocked concurrent Array writes safe): the answer need only be a possible
# one. Reads that bypass the guard funnels are #12814.

plan 3;

my @words = (('a'..'z').list, (('a'..'z') X~ ('a'..'z')).list).flat.list;   # 702 keys

sub hammer(&body) {
    await (^6).map: { start { my $end = now + 0.8; while now < $end { body() } } };
}

class Pusher {
    has @!a;
    method go { @!a.push(1); True }
    method n { @!a.elems }
}
my $p = Pusher.new;
hammer({ $p.go });
ok $p.n > 0, 'concurrent pushes onto an attribute array do not crash';

class Counter {
    has %!h;
    method go($k) { %!h{$k}++; for %!h.kv -> $key, $v { last } }
    method n { %!h.elems }
}
my $c = Counter.new;
hammer({ $c.go(@words.pick) });
ok 0 < $c.n <= 702, 'concurrent %!h{$k}++ and %!h.kv do not crash';

# Single-threaded semantics are untouched.
my $s = Counter.new;
$s.go('x') for ^3;
is $s.n, 1, 'single-threaded increments still land on one key';
