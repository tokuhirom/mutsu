use Test;
use nqp;

# The object-model nqp:: ops (#11499) answer what `.HOW`, `.WHO`, `.WHAT`,
# `.REPR`, `.WHERE`, `.^find_method` and an ordinary call answer, plus the
# Miscellaneous ops `getcodename`, `setdebugtypename` and `takeclosure`. Expected
# answers are Rakudo 2026.09's.

plan 27;

is nqp::how(1).name(1), 'Int', 'how';
ok nqp::how(1) =:= Int.HOW, 'how is the same meta-object .HOW answers';

my $x = 5;
is nqp::what_nd($x).^name, 'Scalar', 'what_nd sees the container';
is nqp::what_nd(5).^name, 'Int', 'what_nd of a bare value is its type';
is nqp::how_nd($x).name(nqp::what_nd($x)), 'Scalar', 'how_nd is the container\'s meta-object';

is nqp::who(Int).^name, 'Stash', 'who';
is nqp::reprname(1), 'P6opaque', 'reprname of an instance';
is nqp::reprname(Int), 'P6opaque', 'reprname of a type object';

my $o = [];
ok nqp::objectid($o) == nqp::objectid($o), 'objectid is stable';
ok nqp::objectid($o) != nqp::objectid([]), 'objectid tells objects apart';

is nqp::findmethod(1, 'Str')(1), '1', 'findmethod on a built-in type';
class A { method foo($n) { 42 + $n } }
is nqp::findmethod(A, 'foo')(A, 1), 43, 'findmethod on a user class';
ok nqp::isnull(nqp::tryfindmethod(1, 'nope')), 'tryfindmethod answers null on a miss';
is nqp::tryfindmethod(1, 'Str')(5), '5', 'tryfindmethod finds a method';
dies-ok { nqp::findmethod(1, 'nope') }, 'findmethod dies on a miss';

is nqp::callmethod(1, 'Str'), '1', 'callmethod without arguments';
is nqp::callmethod('abc', 'substr', 1, 1), 'b', 'callmethod passes arguments';
is nqp::callmethod(A.new, 'foo', 2), 44, 'callmethod on a user class';

is nqp::call(-> $a, $b { $a + $b }, 2, 3), 5, 'call passes arguments';
is nqp::call(sub () { 'none' }), 'none', 'call without arguments';

{
    my $s := sub foo { 42 };
    my $d := nqp::getattr($s, Code, '$!do');
    is nqp::getcodename($d), 'foo', 'getcodename';
    nqp::setcodename($d, 'bar');
    is nqp::getcodename($d), 'bar', 'getcodename reads a setcodename back';
    is $s.name, 'bar', '... and so does Code.name';
}

{
    my class B {}
    ok nqp::setdebugtypename(B, 'Zed') =:= B, 'setdebugtypename answers the type';
    is B.^name, 'B', 'and leaves .^name alone';
}

{
    my $x = 1;
    my $b := { $x };
    my $c := nqp::takeclosure($b);
    $x = 2;
    is $c(), 2, 'a takeclosure closure sees its outer lexical';
    ok nqp::eqaddr($b, $c), 'takeclosure of a block value is the block';
}
