use Test;

# `.wrap` on a multi method's DISPATCHER (the proto `.^method_table<m>` /
# `.^lookup('m')` returns): the wrapper runs once per call, before candidate
# selection, and `callsame`/`callwith` re-dispatches the whole multi.
# Expected values verified against Rakudo (issue #9705, Staticish t/020-test.t).

plan 12;

class A {
    has $.v = 0;
    multi method m(Int $a) { "Int $a v={self.defined ?? $!v !! 'type'}" }
    multi method m(Str $a) { "Str $a v={self.defined ?? $!v !! 'type'}" }
    multi method r(Int $n) { $n <= 0 ?? "done" !! "r$n," ~ self.r($n - 1) }
}

my $h = A.^method_table<m>.wrap(method (|c) { "w:" ~ callsame });
is A.m(1), 'w:Int 1 v=type', 'wrapper runs before the Int candidate';
is A.m('x'), 'w:Str x v=type', 'wrapper runs before the Str candidate';
$h.restore;
is A.m(1), 'Int 1 v=type', 'restore removes the dispatcher wrapper';

# callwith with a new invocant re-dispatches against it (Staticish swaps a
# type-object invocant for its singleton instance).
my $inst = A.new(v => 42);
my $h1 = A.^method_table<m>.wrap(method (|c) { callwith($inst, |c) });
is A.m(2), 'Int 2 v=42', 'callwith re-dispatches on the new invocant';
is A.m('y'), 'Str y v=42', 'callwith re-dispatches on the new invocant (Str)';

my $h2 = A.^lookup('m').wrap(-> $self, |c { "outer[" ~ callsame() ~ "]" });
is A.m(3), 'outer[Int 3 v=42]', 'two dispatcher wrappers nest, newest outermost';
$h2.restore;
A.^method_table<m>.unwrap($h1);
is A.m(4), 'Int 4 v=type', 'unwrap with a handle removes the dispatcher wrapper';

# The wrapper runs once per call: a recursive call from inside a candidate is
# a fresh dispatch and is wrapped again, but the re-dispatch itself is not.
A.^method_table<r>.wrap(method ($n) { "<" ~ callsame() ~ ">" });
is A.new.r(2), '<r2,<r1,<done>>>', 'recursive calls are each wrapped once';

class B is A { }
is B.r(1), '<r1,<done>>', 'a subclass without its own candidates sees the wrapper';

# One call runs the wrapper once, not once per nextsame step.
my $count = 0;
class C {
    multi method n(Int $x) { "Int" }
    multi method n(Cool $x) { "Cool" }
}
C.^lookup('n').wrap(method (|c) { $count++; callsame });
is C.n(1), 'Int', 'the narrowest candidate still wins after the wrapper';
is C.n('s'), 'Cool', 'the wrapper does not fix the candidate before re-dispatch';
is $count, 2, 'the dispatcher wrapper ran once per call';
