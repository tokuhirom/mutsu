use nqp;
use Test;

# #17620: `nqp::eqaddr` compares its operands as given, as rakudo's
# `infix:<=:=>` (which is this op) does: a `$` variable or an element is its
# Scalar container, not the value inside, and a Proxy is not FETCHed. Every
# expected value was measured on rakudo.

plan 21;

sub probe() {
    my @a = 1, 2;
    my $x = @a;
    my $store = @a;
    my $p := Proxy.new(FETCH => method () { $store }, STORE => method ($v) { });
    my $z = IterationEnd;
    my $w := $x;
    my $b := @a;
    my %h = a => @a;
    my @c = @a, 1;
    my $q = 5;
    my $it := IterationEnd;

    is nqp::eqaddr($x, @a), 0, 'a $ variable holding an array is its Scalar';
    is nqp::eqaddr($p, @a), 0, 'a Proxy is not FETCHed';
    is nqp::eqaddr($z, IterationEnd), 0, 'an assigned sentinel sits in a Scalar';
    is nqp::eqaddr($q, 5), 0, 'an assigned Int sits in a Scalar';
    is nqp::eqaddr(%h<a>, @a), 0, 'a hash element is its container';
    is nqp::eqaddr($x, $x), 1, 'a Scalar is itself';
    is nqp::eqaddr($w, $x), 1, 'a := alias is the same Scalar';
    is nqp::eqaddr($p, $p), 1, 'a Proxy is itself';
    is nqp::eqaddr(@c[0], @c[0]), 1, 'an element container is itself';
    is nqp::eqaddr($b, @a), 1, 'a := binding of an array is the array';
    is nqp::eqaddr($it, IterationEnd), 1, 'a := binding of the sentinel is the sentinel';
    is nqp::eqaddr(nqp::decont($x), @a), 1, 'nqp::decont reaches the array';
    is nqp::eqaddr(Nil, Nil), 1, 'Nil is Nil';
}
probe();

{
    my @a = 1, 2;
    my $x = @a;
    is nqp::eqaddr($x, @a), 0, 'the mainline answers the same';
    is +($x =:= @a), nqp::eqaddr($x, @a), '=:= and nqp::eqaddr agree';
}

# A user `.WHICH` makes two objects `===`, never the same object.
{
    my class C { method WHICH { ValueObjAt.new("C|1") } }
    is nqp::eqaddr(C.new, C.new), 0, 'eqaddr ignores a user WHICH';
    nok C.new =:= C.new, '=:= ignores a user WHICH';
}

# The same answers from a small routine called many times (the typed tier).
sub assigned() { my $v = 5; nqp::eqaddr($v, 5) }
sub same-var() { my $v = 5; nqp::eqaddr($v, $v) }
sub values() { nqp::eqaddr(Order::Less, (1 cmp 2)) }
my ($n-assigned, $n-same, $n-values) = 0, 0, 0;
for ^200 {
    $n-assigned += assigned();
    $n-same += same-var();
    $n-values += values();
}
is $n-assigned, 0, 'a hot routine still sees the Scalar';
is $n-same, 200, 'a hot routine still sees the same Scalar';
is $n-values, 200, 'a hot routine compares plain values';

# The iteration idiom binds, so it compares the pulled value.
{
    my $it := (1, 2, 3).iterator;
    my $n = 0;
    nqp::until(nqp::eqaddr((my $got := $it.pull-one), IterationEnd), $n++);
    is $n, 3, 'a := bound pull-one result reaches IterationEnd';
}
