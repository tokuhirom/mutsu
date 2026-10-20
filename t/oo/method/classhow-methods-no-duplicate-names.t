use Test;

# `.^methods` lists one entry per method name; a method a built-in class
# shares with a role it composes (Int/Real, Map/Iterable) is not repeated.

plan 12;

for Int, Str, Map, Num, Rat, Range -> \t {
    my @n = t.^methods.map(*.name);
    is @n.repeated.unique.join(","), "", "{t.^name}.^methods has no repeated name";
}

for Int, Map {
    my @l = .^methods(:local).map(*.name);
    is @l.repeated.elems, 0, "{.^name}.^methods(:local) has no repeated name";
}

is Int.^methods.map(*.name).elems, Int.^methods.map(*.name).unique.elems,
    "Int: entries equal unique names";
is Map.^methods.grep(*.name eq 'hyper').elems, 1, "Map lists hyper once";
ok Int.^find_method('sqrt').defined, "Int.sqrt is still found";
ok Map.^methods.grep(*.name eq 'lazy').elems == 1, "Map lists lazy once";

done-testing;
