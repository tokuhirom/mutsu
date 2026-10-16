use Test;

# A `%`/`@` variable bound to an immutable value (a Set, a Bag, a Map, a List)
# and rebound by a closure another routine runs: the variable takes a binding
# cell at its declaration whatever it holds, so the rebind reaches the
# declaring block and every closure that captured it, without a frame's return
# merge carrying it (ADR-12529 §3, slice 29). Checked against rakudo.

plan 6;

sub run(&c) { c() }

{
    my %qh := Set.new(1, 2);
    my &peek = { %qh.elems };
    run { %qh := Set.new(3, 4, 5) };
    is-deeply %qh.keys.sort.List, (3, 4, 5), 'a rebound Set reaches the block';
    is peek(), 3, 'and a closure made before the rebind';
}

{
    my %b := bag <a a b>;
    run { %b := bag <c> };
    is %b.keys.join, 'c', 'a Bag rebind';
}

{
    my %m := Map.new((x => 1));
    run { %m := Map.new((y => 2)) };
    is %m<y>, 2, 'a Map rebind';
}

{
    my @l := (1, 2);
    run { @l := (7, 8, 9) };
    is @l.elems, 3, 'a List rebind';
}

{
    my %qh := Set.new(1);
    run { %qh := Set.new(2) } for ^2;
    is %qh.keys.join, '2', 'repeated rebinds from fresh closures';
}
