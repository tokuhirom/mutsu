use Test;

# `self!m(...) = $v` where `!m` is an `is rw` private method is an lvalue call,
# exactly like its public twin: the method runs and the assignment writes
# through the container it hands back. mutsu used to treat the `!m` name as a
# private *attribute* and store an attribute named `m` (instance invocant), or
# fall to the setter chain (type-object invocant), so the write vanished.
# Reduced from the BigRoot distribution's result cache.

plan 7;

class Store {
    has %.h;
    method !slot($k) is rw { %!h{$k}{1} }
    method put($k, $v) { self!slot($k) = $v; %!h }
    method !cache($k) is rw { state %c; %c{$k}{2} }
    method remember($k, $v) { self!cache($k) = $v; self!cache($k) }
    method !counter() is rw { state $n }
    method bump() { self!counter() = 9; self!counter() }
    method !plain() { 5 }
    method bad() { self!plain() = 1 }
}

is-deeply Store.new.put(2, 9), {2 => {1 => 9}}, 'instance: writes an autovivified attribute hash element';
is Store.new.remember('a', 7), 7, 'instance: writes a state hash element';
is Store.remember('b', 8), 8, 'type object: writes a state hash element';
is Store.bump, 9, 'type object: writes a state scalar returned by the tail';

class Priv {
    has $!foo;
    method !foo is rw { $!foo }
    method set($v) { self!foo = $v; $!foo }
}
is Priv.new.set(3), 3, 'a private rw accessor over an attribute still writes it';

throws-like { Store.bad }, X::Assignment::RO, 'a non-rw private method refuses the assignment';

# The `without` form BigRoot uses: fill the cache slot only when it is empty.
class Cache {
    method !result($k) is rw { state %r; %r{$k}{1} }
    method get($k) {
        without self!result($k) {
            self!result($k) = $k * 10;
        }
        self!result: $k
    }
}
is Cache.get(4), 40, 'without over a private rw call fills the slot';
