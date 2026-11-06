use Test;

# A `proto method` hands its call to the winning multi candidate, and a
# candidate that binds its invocant `is rw` still writes through to the caller's
# variable: `$heap.push(...)` on an uninitialized typed variable autovivifies it
# (BinaryHeap's `multi method push(::?ROLE:U $self is rw: ...)`).
plan 12;

class K {
    has $.v;
    proto method fill(|) {*}
    multi method fill(K:U $self is rw: $x) { $self = K.new(v => $x); }
    multi method fill(K:D: $x) { $!v = $x; self }
}
my K $k;
$k.fill(1);
ok $k.defined, 'proto method: an is rw type-object invocant is assigned';
is $k.v, 1, 'and holds the constructed object';

my K $k2;
my $r = $k2.fill(2);
ok $r === $k2, 'the returned object is the autovivified variable';

role H {
    has @.a;
    proto method push(| --> ::?CLASS:D) {*}
    multi method push(::?ROLE:U $self is rw: **@values is raw) {
        $self = self.CREATE.push(|@values);
    }
    multi method push(::?ROLE:D: **@values is raw) { @!a.append(@values); self }
}
class M does H { }
my M $h;
my $ret = $h.push(1, 2, 3);
ok $h.defined, 'role proto method: the typed variable is autovivified';
ok $ret === $h, 'the call returns the very object the variable holds';
is $h.a.join(','), '1,2,3', 'and the values were pushed';
$h.push(4);
is $h.a.join(','), '1,2,3,4', 'a later call on the now defined variable appends';

class L {
    has $.v;
    proto method fill(|) {*}
    multi method fill($self is rw: $x) { $self = L.new(v => $x); }
}
my L $l;
$l.fill(7);
ok $l.defined, 'untyped explicit rw invocant under a proto';
is $l.v, 7, 'holds the new object';

class N {
    has $.v;
    proto method fill(|) {*}
    multi method fill(N:U $self is rw: Int $x) { $self = N.new(v => $x); }
    multi method fill(N:U $self is rw: Str $x) { $self = N.new(v => $x.chars); }
}
my N $n1;
$n1.fill(5);
is $n1.v, 5, 'candidate chosen by the argument type (Int)';
my N $n2;
$n2.fill("abc");
is $n2.v, 3, 'candidate chosen by the argument type (Str)';
my N $n3;
lives-ok { $n3.fill(1) }, 'a second variable autovivifies independently';
