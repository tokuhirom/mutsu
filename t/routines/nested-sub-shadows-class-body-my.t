use Test;

plan 6;

# A nested `sub` reading a variable its enclosing method declares must see that
# binding, not a same-named `my` in the class body (#15986).
class P {
    method go($alpn) { sub h() { $alpn }; h() }
    my $alpn = 'class-body';
}
is P.go('param'), 'param', 'nested sub sees the enclosing method parameter';

class L {
    method go() { my $alpn = 'local'; sub h() { $alpn }; h() }
    my $alpn = 'class-body';
}
is L.go(), 'local', 'nested sub sees the enclosing method local';

class N {
    method go(:$alpn) { sub h() { $alpn }; h() }
    my $alpn;
}
is-deeply N.go(:alpn(<h2 http/1.1>)), <h2 http/1.1>, 'named parameter shadows the class-body my';

class S {
    method !setup($source, :$alpn) {
        supply {
            whenever $source -> $v { handle($v) }
            sub handle($v) { emit $alpn }
        }
    }
    method listen($source, :$alpn) { self!setup($source, :$alpn) }
    my $alpn;
    method supports { $alpn //= True }
}
my $sup = Supplier.new;
my @got;
S.listen($sup.Supply, :alpn(<h2 x>)).tap({ @got.push($_) });
$sup.emit(1);
sleep 0.2;
is @got.map(*.join(',')).join(';'), 'h2,x', 'nested sub in a supply block sees the method parameter';

# The class-body variable itself is still reachable where nothing shadows it.
class K {
    my $count = 0;
    method bump() { sub inc() { $count++ }; inc(); inc(); $count }
}
is K.bump(), 2, 'a nested sub still reaches the class-body my it does not shadow';
is S.supports, True, 'the class-body variable is its own';
