use Test;

plan 4;

# An `is default(...)` value belongs to the attribute's container, so it holds
# while BUILD runs, before any initializer (Pakku::Log's `has $!warn is default(Nil)`).
class A {
    has $!w is default(Nil);
    has $.seen;
    submethod BUILD(:$x) { $!seen = $!w.WHAT.^name }
}
is A.new(:x(1)).seen, 'Nil', 'is default(Nil) holds inside BUILD';

class B {
    has $!w is default(Nil);
    submethod BUILD { $!w.msg: :a(1) }
}
lives-ok { B.new }, 'a method call on the Nil default is Nil, not a failure';

class C {
    has Int $.n is default(7);
    has $.seen;
    submethod BUILD { $!seen = $!n }
}
is C.new.seen, 7, 'a numeric is default(...) holds inside BUILD';
is C.new.n, 7, 'and after construction';
