use v6;
use Test;

plan 6;

# When a class declares user multi `new` candidates that don't match a
# named-args call, Mu.new(*%attrinit) is still available as a fallback —
# including from a concrete invocant (URI::Path's `self.new: :$path`).
class A {
    has $.x;
    multi method new(A:U: Int $s) { self.new(x => $s) }
    method go() { self.new(x => 9) }
}

is A.new(x => 5).x, 5, 'named-args new falls back to the default constructor';
is A.new(7).x, 7, 'the user multi candidate still dispatches';
is A.new(x => 5).go.x, 9, 'self.new(:named) from a :D invocant falls back too';

# An explicit proto owns dispatch: no default-constructor fallback.
class P {
    has $.x;
    proto method new($) {*}
    multi method new(Int $s) { self.bless(x => $s) }
}
is P.new(3).x, 3, 'proto-owned multi new dispatches';
dies-ok { P.new(x => 3) }, 'proto-owned new does not fall back to Mu.new';

# Positional args with no matching candidate still die.
class R {
    has $.x;
    multi method new(R:U: Int $s) { self.new(x => $s) }
}
dies-ok { R.new('str', 'extra') }, 'unmatched positional args still die';
