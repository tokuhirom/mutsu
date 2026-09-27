use Test;

# `=:=` compares containers (#9769): an assigned `$x` owns a Scalar, so it is
# never the same container as a literal or a type-object term, while a `:=`
# binding makes the name the value itself.

plan 13;

{
    my $x = 1;
    nok $x =:= 1, 'an assigned scalar is not a literal';
    nok 1 =:= $x, 'the literal on the left is the same';
}
{
    my $a = 1;
    my $b = 1;
    nok $a =:= $b, 'two scalars holding equal values are different containers';
    my $c := $a;
    ok $c =:= $a, 'a bound alias is the same container';
    ok $a =:= $a, 'a scalar is its own container';
}
{
    my $x := 1;
    ok $x =:= 1, 'a scalar bound to a literal is that literal';
}
{
    my $x = IterationEnd;
    nok $x =:= IterationEnd, 'an assigned IterationEnd is not the sentinel';
    my $y := IterationEnd;
    ok $y =:= IterationEnd, 'a bound IterationEnd is the sentinel';
}
{
    my $x;
    nok $x =:= Any, 'an unassigned scalar is not the Any type object';
}
{
    my $x = 1;
    my \t := $x;
    ok $x =:= t, 'a sigilless alias is still a variable, not a bare value';
}
{
    constant C = 5;
    my $x := C;
    ok $x =:= C, 'a scalar bound to a constant is that constant';
}
{
    my $it = (1, 2).iterator;
    my $n = 0;
    until (my $v := $it.pull-one) =:= IterationEnd { $n++ }
    is $n, 2, 'the pull-one loop idiom still stops at IterationEnd';
}
{
    sub f($p) { $p =:= 1 }
    nok f(1), 'a readonly parameter owns a container';
}
