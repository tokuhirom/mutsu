use v6;
use Test;

# A class's body `my` static (`class C { my $x = ...; method m { $x } }`) must
# stay reachable from the class's methods even when the class is defined inside a
# transient frame (a sub, or a runtime `require`) whose env is gone by the time a
# method runs. Regression: an initialized `my Lock $lock = Lock.new` read back as
# `Any` inside such a method (the zef fetch-plugin `!command` lock blocker).

plan 8;

# 1-2. Initialized static read after the defining sub returned.
sub make-locked() {
    class WithLock {
        my Lock $lock = Lock.new;
        method lock-name { return $lock.^name }
    }
    return WithLock.new;
}
my $wl = make-locked();
is $wl.lock-name, 'Lock', 'initialized class-body static keeps its value after the defining sub returns';
is $wl.lock-name, 'Lock', 'still correct on a second call';

# 3-5. Mutable static persists across method calls (accumulator).
sub make-counter() {
    class Counter {
        my Int $n = 0;
        method inc { $n++; return $n }
    }
    return Counter.new;
}
my $c = make-counter();
is $c.inc, 1, 'mutable class-body static: first increment';
is $c.inc, 2, 'mutable class-body static: persists across calls';
is $c.inc, 3, 'mutable class-body static: third increment';

# 6-8. The static is shared across instances (Raku semantics), even for a
# class defined inside a sub.
sub make-shared-pair() {
    class Shared {
        my Int $count = 0;
        method bump { $count++; return $count }
    }
    return (Shared.new, Shared.new);
}
my ($x, $y) = make-shared-pair();
is $x.bump, 1, 'shared static across instances: first';
is $y.bump, 2, 'shared static across instances: second instance sees the same static';
is $x.bump, 3, 'shared static across instances: third';
