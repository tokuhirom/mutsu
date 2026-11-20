use Test;
plan 4;

# A method call's samewith context and its pending deferral entry share one
# copy of the call's arguments (#18267): `samewith` restarts dispatch with
# them, and each call's `callsame` sees its own.
class Counter {
    multi method f(Int $n where * > 0) { samewith($n - 1) + 1 }
    multi method f(Int $n) { 0 }
}
is Counter.new.f(3), 3, 'samewith restarts dispatch on an ordinary class';

class Base { method greet($x) { "base:$x" } }
class Kid is Base { method greet($x) { 'kid>' ~ callsame } }
is Kid.new.greet('a'), 'kid>base:a', 'callsame sees the call arguments';
is Kid.new.greet('b'), 'kid>base:b', 'a second call sees its own arguments';

class Twice {
    multi method g(Str $s) { samewith($s.chars) }
    multi method g(Int $n) { $n * 2 }
}
is Twice.new.g('abcd'), 8, 'samewith from a method redispatches with new arguments';
