use Test;

# A call through a trivial proto remembers its winner per argument types
# (#15786, Concurrent::PriorityQueue / Array::Sorted::Util). The memo must
# answer exactly what a fresh resolution would.

plan 12;

proto sub f(|) {*}
multi sub f(Int $x) { "Int" }
multi sub f(Str $x) { "Str" }
multi sub f(Int $x, :$force!) { "Int+force" }

is (f(1), f("a"), f(2), f("b")).join(","), "Int,Str,Int,Str", 'alternating argument types';
is f(3, :force), "Int+force", 'a named argument picks its candidate';
is f(3), "Int", 'and without it the other one again';

proto sub g(|) {*}
multi sub g(Int $x where * > 10) { "big" }
multi sub g(Int $x) { "small" }
is (g(5), g(50), g(6), g(60)).join(","), "small,big,small,big",
    'a value-dependent family is decided per value';

subset Even of Int where * %% 2;
proto sub h(|) {*}
multi sub h(Even $x) { "even" }
multi sub h(Int $x) { "odd" }
is (h(2), h(3), h(4)).join(","), "even,odd,even", 'a subset-typed family too';

proto sub k(|) {*}
multi sub k(Int $x) { "outer" }
is k(1), "outer", 'before a block-scoped redefinition';
{
    proto sub k(|) {*}
    multi sub k(Int $x) { "inner" }
    is k(1), "inner", 'inside the block';
}
is k(1), "outer", 'after the block';

my @calls;
proto sub m(|) {*}
multi sub m(@a, $n, :&cmp = &[cmp]) { @calls.push(cmp($n, @a[0])); "pos" }
multi sub m(@a, $n, **@also, :&cmp = &[cmp]) { "also" }
is m([1], 1), "pos", 'a defaulted code parameter';
is m([1], 1, 2, 3), "also", 'a slurpy candidate';
is m([1], 1, :cmp(-> $a, $b { Less })), "pos", 'a passed code parameter';
is @calls.map(*.Str).join(","), "Same,Less", 'each call used its own &cmp';
