use Test;

plan 8;

# `.new` takes its arguments as elements: a Pair is an ordinary element, not a
# key => weight (only `.new-from-pairs` reads Pairs that way).
for <Set SetHash Bag BagHash Mix MixHash> -> $name {
    my $type = ::($name);
    is $type[Pair].new("a" => 0, "b" => 1).keys.map(*.^name).sort.join(","),
        "Pair,Pair", "$name\[Pair].new keeps Pair elements";
}
throws-like { Set[Int].new("a" => 1) }, X::TypeCheck::Binding,
    'Set[Int].new(Pair) type-checks the Pair itself';
is Set[Pair].new("a" => 0).elems, 1, 'a zero-valued Pair is still an element';
