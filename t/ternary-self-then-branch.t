use v6;
use Test;

# `self` as the then-branch of a ternary must be accepted. The then-branch
# guard treats a bare identifier as the (incomplete) head of a listop call and
# errors, but `self` is a complete nullary term, like a type object. Regression
# surfaced by Pray::Geometry::Vector3D (`... ?? self.scale(...) !! $in ?? self !! self.clone`).

plan 5;

class V {
    has $.n = 1;
    method clone-ish { V.new(n => $!n) }
    method pick-self($cond)  { $cond ?? self !! V.new(n => 99) }
    method pick-other($cond) { $cond ?? V.new(n => 99) !! self }
    method chained($a, $b)   { $a ?? self !! ($b ?? self !! self.clone-ish) }
}

my $v = V.new(n => 5);
is $v.pick-self(True).n,  5,  "self as then-branch returns the invocant";
is $v.pick-self(False).n, 99, "false then-branch takes the else";
is $v.pick-other(True).n, 99, "self as else-branch still works";
is $v.chained(True, False).n, 5, "self in a nested-ternary then-branch";
is $v.chained(False, False).n, 5, "self.method reached through the else chain";
