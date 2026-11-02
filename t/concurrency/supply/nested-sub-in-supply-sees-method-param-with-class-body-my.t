use Test;

plan 2;

# A named sub declared in a `supply` block of a method, called from a
# `whenever` and also reading a supply-local variable, must see the method's
# parameter, not the same-named class-body `my` (#16077).
class C {
    method !setup($source, :$alpn) {
        supply {
            my $ctx = 1;
            whenever $source -> $s { h() }
            sub h() { emit ($ctx, $alpn.raku).join(",") }
        }
    }
    method listen($source, :$alpn) { self!setup($source, :$alpn) }
    my $alpn;
}
my $sup = Supplier.new;
my @got;
C.listen($sup.Supply, :alpn(<h2 x>)).tap({ @got.push($_) });
$sup.emit(1);
sleep 0.3;
is @got.join(';'), '1,$("h2", "x")', 'nested sub sees the method parameter';

class D {
    method go(:$alpn) { my $ctx = 2; sub h() { "$ctx|$alpn" }; h() }
    my $alpn = 'class-body';
}
is D.go(:alpn<p>), '2|p', 'plain method nested sub still sees the parameter';
