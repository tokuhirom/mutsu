use Test;

plan 9;

sub req-opt($a, $b?) { }
is &req-opt.arity, 1, 'arity counts required positional params only';
is &req-opt.count, 2, 'count includes optional positional params';

sub named-required($a, :$b!) { }
is &named-required.arity, 1, 'required named params do not affect arity';
is &named-required.count, 1, 'named params do not affect count';

is (-> *@a { }).count, Inf, 'slurpy positional pointy param has infinite count';
is (-> *%a { }).count, 0, 'slurpy named pointy param does not increase count';

{
    my proto sub a($, $?) { * }
    my multi sub a($) { 1 }
    my multi sub a($, $) { 2 }
    is &a.arity, 1, 'multi arity is minimum required positional count';
    is &a.count, 2, 'multi count is maximum positional count';
}

{
    class A {
        our method f ($x: $y) { $y * 2 }
    }
    my &g = &A::f.assuming(A.new);
    is g(3), 6, 'assuming on our method with explicit invocant binds correctly';
}
