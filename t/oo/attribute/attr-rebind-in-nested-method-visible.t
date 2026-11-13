use Test;

plan 7;

# A frame that read a `:=`-bound scalar attribute keeps seeing it after a
# nested method REBINDS it: the attribute is read from the instance, not from
# the cell the frame saw first (ForwardIterables' `next-iterator`).

class A {
    has $!c;
    method n($v) { $!c := $v; self }
    method a { my @r; @r.push: $!c; self.n(2); @r.push: $!c; @r }
}
is-deeply A.new.n(1).a, [1, 2], 'rebind of a bound attribute in a nested method';

class B {
    has $!c;
    method n($v) { $!c := $v + 0; self }
    method a { my @r; @r.push: $!c; self.n(2); @r.push: $!c; @r }
}
is-deeply B.new.n(1).a, [1, 2], 'rebind to a computed value';

class C {
    has $!c;
    method n(\v) { $!c := v; self }
    method a { my @r; @r.push: $!c; self.n(2); @r.push: $!c; self.n(3); @r.push: $!c; @r }
}
is-deeply C.new.n(1).a, [1, 2, 3], 'rebinding twice through a sigilless parameter';

class D {
    has $!its;
    has $!cur;
    method setup(*@x) { $!its := @x.List.iterator; self.nx }
    method nx() { $!cur := $!its.pull-one; self }
    method all() { my @r; @r.push: $!cur; self.nx; @r.push: $!cur; self.nx; @r.push: $!cur; @r }
}
is-deeply D.new.setup(<a b c>).all, [<a b c>], 'an iterator-advancing method seen by its caller';

class E {
    has $!c;
    method n($v) { $!c := $v; self }
    method a {
        my $x = 10;
        $!c := $x;
        my @r = $!c;
        $x = 11;
        @r.push: $!c;
        @r
    }
}
is-deeply E.new.n(1).a, [10, 11], 'a bind in this frame keeps tracking its container';

class F {
    has $!c = 0;
    method bump { $!c = $!c + 1; self }
    method a { my @r; @r.push: $!c; self.bump; @r.push: $!c; @r }
}
is-deeply F.new.a, [0, 1], 'an assignment in a nested method is still seen';

use nqp;
class G does Iterator {
    has $!its;
    has $!cur;
    method new(*@its) { my $s := nqp::create(self); nqp::bindattr($s, G, '$!its', @its.List.iterator); $s.nx }
    method nx() { my $n := $!its.pull-one; $!cur := $n =:= IterationEnd ?? Nil !! $n.iterator; self }
    method pull-one() is raw {
        my $p := $!cur.pull-one;
        $p =:= IterationEnd && self.nx && $!cur.defined ?? self.pull-one !! $p
    }
}
is-deeply Seq.new(G.new((1, 2), (3, 4))).List, (1, 2, 3, 4), 'chained iterators through a Seq';
