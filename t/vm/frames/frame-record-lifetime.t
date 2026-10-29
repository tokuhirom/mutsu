use Test;

# ADR-14843: a frame's record lives exactly as long as the frame. Recursion,
# exceptions, return and loop exits all restore the caller's `self`.
# Expected values are raku's.

plan 9;

class L {
    has $.id is required;
    has @.seen;
    method rec($n) { return $!id if $n == 0; self.rec($n - 1) }
    method other($o) { $o.id ~ self.id }
    method after-other($o) { $o.id; self.id }
    method thrower { die "x" }
    method after-throw { try self.thrower; self.id }
    method early($x) { return self.id if $x; 'no' }
    method loop-exit { for 1..3 { last if $_ == 2 }; self.id }
    method deep { L.new(id => 'inner').rec(3) ~ self.id }
    method lives-on { my $s = self; -> { $s.id } }
}

my $l = L.new(id => 'a');
is $l.rec(5), 'a', 'recursion keeps self';
is $l.other(L.new(id => 'b')), 'ba', 'a call with another instance';
is $l.after-other(L.new(id => 'b')), 'a', 'self after the call returned';
is $l.after-throw, 'a', 'self after a caught exception';
is $l.early(True), 'a', 'self at an early return';
is $l.loop-exit, 'a', 'self after a loop exit';
is $l.deep, 'innera', 'a nested call on a fresh object';
is $l.lives-on.(), 'a', 'a closure capturing self outlives the frame';
{
    my $r = L.new(id => 'r');
    is $r.rec(2), 'r', 'a fresh object after all of the above';
}
