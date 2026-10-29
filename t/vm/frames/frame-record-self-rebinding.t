use Test;
use MONKEY-TYPING;

# ADR-14843: `self` lives in the method frame's record, so every way of
# rebinding or writing it goes through the record, not an Env key. Expected
# values are raku's.

plan 14;

class Counter {
    has $.n is rw = 0;
    method bump { $!n++; self }
    method both($other) { self.n + $other.n }
    method nested { Counter.new(n => 10).bump.n + self.n }
    method after-call { self.nested; self.n }
    method temp-attr { temp self.n = 99; self.n }
    method temp-dot  { temp $.n = 5; $.n }
}

my $c = Counter.new(n => 1);
is $c.bump.bump.n, 3, 'a chained call answers the invocant';
is $c.both(Counter.new(n => 4)), 7, 'self and a second instance do not mix';
is $c.nested, 14, 'a call on a fresh instance inside a method keeps self';
is $c.after-call, 3, 'self is the same after a nested call returned';
is $c.temp-attr, 99, 'temp self.attr inside the scope';
is $c.n, 3, '... restored on exit';
is $c.temp-dot, 5, 'temp $.attr inside the scope';
is $c.n, 3, '... restored on exit';

# a mutating call on `self` reaches the caller's variable
augment class Hash {
    method add-twice($k, $v) { self.append($k => $v); self.append($k => $v) }
    method store-key($k, $v) { self{$k} = $v; self }
    method drop-key($k)      { self{$k}:delete }
}
my %h;
%h.add-twice('a', 1);
is-deeply %h, {a => [1, 1]}, 'self.append on a Hash writes through to the caller';
my %g;
%g.store-key('k', 5);
is-deeply %g, {k => 5}, 'self{$k} = v writes through to the caller';
is %g.drop-key('k'), 5, 'self{$k}:delete answers the deleted value';
is-deeply %g, {}, '... and removes it from the caller';

# an exception leaves the caller's self intact
class Thrower {
    has $.tag = 'outer';
    method boom { die 'x' }
    method guarded { try self.boom; $!tag }
}
is Thrower.new.guarded, 'outer', 'self survives a caught exception';

# a method literal called as a function binds its first argument as self
my $m = method () { self.^name };
is 3.$m, 'Int', 'a method literal takes the receiver as self';
