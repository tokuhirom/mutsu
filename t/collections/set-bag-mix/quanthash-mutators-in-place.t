use Test;

# SetHash.set/.unset and the QuantHash .grab/.grabpairs mutate the object
# itself, so every holder sees the change: an attribute, an accessor result,
# an element, an alias (#9609).

plan 22;

{
    my class C {
        has $!q = SetHash.new("x", "y");
        method g { $!q.unset("x"); $!q.keys.sort.List }
    }
    is-deeply C.new.g, ("y",), 'unset on an attribute is visible in the same method';
}

{
    my class D {
        has $!q = SetHash.new("x");
        method g { $!q.grab }
        method n { $!q.elems }
    }
    my $d = D.new;
    is $d.g, "x", 'grab on an attribute returns the key';
    is $d.n, 0, 'grab on an attribute removes the key';
}

{
    my class E { has $.q = SetHash.new("x") }
    is E.new.q.grab, "x", 'grab through an accessor';
    my $e = E.new;
    $e.q.set("w");
    is-deeply $e.q.keys.sort.List, <w x>, 'set through an accessor';
    $e.q.unset("x");
    is-deeply $e.q.keys.sort.List, ("w",), 'unset through an accessor';
}

{
    my class F {
        has $.q = SetHash.new("x");
        method s { $!q.set("z"); $!q.set(<p q>) }
    }
    my $f = F.new;
    $f.s;
    is-deeply $f.q.keys.sort.List, <p q x z>, 'set with a list argument on an attribute';
}

{
    my class B {
        has $!b = BagHash.new(<a a b>);
        method g { $!b.grab(*) }
        method n { $!b.elems }
    }
    my $b = B.new;
    is-deeply $b.g.sort.List, <a a b>, 'BagHash grab(*) on an attribute';
    is $b.n, 0, 'BagHash grab(*) drained the attribute';
}

{
    my class M {
        has $!m = MixHash.new(<a b>);
        method g { $!m.grabpairs(*) }
        method n { $!m.elems }
    }
    my $m = M.new;
    is $m.g.elems, 2, 'MixHash grabpairs(*) on an attribute';
    is $m.n, 0, 'MixHash grabpairs(*) drained the attribute';
}

{
    my $a = SetHash.new(<a>);
    my $alias = $a;
    $alias.set("z");
    is-deeply $a.keys.sort.List, <a z>, 'set through an alias reaches the original';
    $alias.unset("a");
    is-deeply $a.keys.sort.List, ("z",), 'unset through an alias reaches the original';
}

{
    my @l;
    @l.push: SetHash.new(<k l>);
    @l[0].unset("k");
    is-deeply @l[0].keys.List, ("l",), 'unset on an array element';
    my %h = a => SetHash.new(<k l>);
    %h<a>.grab(*);
    is %h<a>.elems, 0, 'grab(*) on a hash value';
}

{
    my $g = SetHash.new(<a b c d>);
    is $g.grab(* - 1).elems, 3, 'grab with a Callable count';
    is $g.elems, 1, 'grab with a Callable count removed that many keys';
}

# A coercion across mutability never shares storage with the source.
{
    my $sh = SetHash.new(<a>);
    my $s = $sh.Set;
    $sh.set("q");
    is-deeply $s.keys.List, ("a",), '.Set is not changed by a later SetHash.set';

    my $set = set <p>;
    my $h = $set.SetHash;
    $h.set("r");
    is-deeply $set.keys.List, ("p",), 'Set.SetHash.set leaves the Set alone';

    my $bag = bag <a b>;
    my $bh = $bag.BagHash;
    $bh.grab(*);
    is $bag.elems, 2, 'Bag.BagHash.grab leaves the Bag alone';

    my $imm = set <a>;
    my %t is SetHash = $imm;
    %t.grab;
    is $imm.elems, 1, '`is SetHash` initialized from a Set does not share it';
    is %t.elems, 0, '... while the SetHash itself was grabbed from';
}
