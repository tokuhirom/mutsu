use Test;

# From the Hash::MutableKeys distribution: a closure returned from a role
# method must see the role parameter of ITS class's specialization, not the
# value the first composition left behind in the composing scope.

plan 4;

role R[$m = "push"] {
    my $x = 1;
    method esc() { -> $y { $m ~ $y } }
    method direct() { $m }
}
class A does R["append"] {}
class B does R {}

is A.new.esc()(1), 'append1', 'escaping closure, first specialization';
is B.new.esc()(1), 'push1', 'escaping closure, default specialization';
is A.new.direct, 'append', 'direct read, first specialization';
is B.new.direct, 'push', 'direct read, default specialization';

done-testing;
