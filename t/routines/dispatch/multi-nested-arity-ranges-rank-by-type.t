use v6;
use Test;

plan 13;

# Two fixed-arity candidates whose positional ranges nest are ordered by their
# parameter types; only an overlap without nesting is incomparable.
multi p1($a?, $b?) { "any" }
multi p1(Int $c) { "int" }
multi p2($a, $b?, $c?) { "any" }
multi p2(Int $a, Int $b) { "int2" }
multi p3($a?, $b?, $c?, $d?) { "any" }
multi p3(Int $a, Int $b) { "int2" }
multi p4(&block) { "code" }
multi p4($a?, $b?) { "any" }

is p1(3), "int", 'optional-only candidate against a typed single';
is p1("x"), "any", 'a non-matching type falls back';
is p1(), "any", 'no arguments';
is p2(1, 2), "int2", '[1..3] against [2..2]';
is p3(1, 2), "int2", '[0..4] against [2..2]';
is p4({ 1 }), "code", 'a Callable beats untyped optionals';

# Overlap without nesting stays ambiguous.
multi q1(Int $a, Int $b, $c?) { "A" }
multi q1(Any $a, Any $b, Any $t, $d?) { "B" }
throws-like { q1(1, 2, 3) }, X::Multi::Ambiguous, '[2..3] against [3..4] is ambiguous';

# The same for multi methods called on `self` from inside the class.
class K {
    multi method set($first?, $second?) { "any" }
    multi method set(&block) { "code" }
    multi method set(Int $n) { "int" }
    method via-self-code() { self.set({ 3 }) }
    method via-self-int() { self.set(3) }
    method via-self-str() { self.set("s") }
}
is K.new.set({ 1 }), "code", 'method, called from outside';
is K.new.via-self-code, "code", 'method, called on self with a block';
is K.new.via-self-int, "int", 'method, called on self with an Int';
is K.new.via-self-str, "any", 'method, called on self with a Str';

class L {
    multi method set($first?, $second?) { "any" }
    multi method set(&block) { "code" }
    submethod TWEAK(:&block) { $!seen = self.set: $_ with &block }
    has $!seen;
    method seen() { $!seen }
}
is L.new(block => { 1 }).seen, "code", 'TWEAK dispatching on a named Callable (Selkie::UI shape)';
is L.new.seen, Any, 'and nothing without it';
