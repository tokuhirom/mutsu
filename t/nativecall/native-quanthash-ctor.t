use Test;

# §D state-ownership: the VM constructs the QuantHash family
# (Set/SetHash/Bag/BagHash/Mix/MixHash) `.new` natively via the single
# `try_native_quanthash_construct` impl the interpreter's dispatch_new also
# delegates to — no interpreter `.new` bounce, behavior identical.

plan 18;

# --- Set / SetHash ---
{
    my $s = Set.new(1, 2, 2, 3);
    isa-ok $s, Set, 'Set.new returns a Set';
    is $s.elems, 3, 'Set dedups elements';
    ok 2 ∈ $s, 'Set membership';

    my $sh = SetHash.new(<a b b>);
    isa-ok $sh, SetHash, 'SetHash.new returns a SetHash';
    is $sh.elems, 2, 'SetHash dedups';
}

# --- Bag / BagHash ---
{
    my $b = Bag.new(<a a b>);
    isa-ok $b, Bag, 'Bag.new returns a Bag';
    is $b{'a'}, 2, 'Bag counts duplicates';
    is $b.total, 3, 'Bag total';

    my $bh = BagHash.new(1, 1, 1);
    isa-ok $bh, BagHash, 'BagHash.new returns a BagHash';
    is $bh{1}, 3, 'BagHash counts';
}

# --- Mix / MixHash ---
{
    my $m = Mix.new(1, 1, 2);
    isa-ok $m, Mix, 'Mix.new returns a Mix';
    is $m.total, 3, 'Mix total';

    my $mh = MixHash.new(<x y y>);
    isa-ok $mh, MixHash, 'MixHash.new returns a MixHash';
    is $mh{'y'}, 2, 'MixHash weight';
}

# --- parameterized variants keep their declared type ---
{
    is Bag[Int].new(1, 2, 2).^name, 'Bag[Int]', 'parameterized Bag name';
    is Set[Int].new(1, 2).^name, 'Set[Int]', 'parameterized Set name';
}

# --- a parameterized element type-check failure throws X::TypeCheck::Binding ---
{
    throws-like { Set[Int].new("a") }, X::TypeCheck::Binding,
        'parameterized Set type-check failure throws';
}

# --- a QuantHash passed as a single arg is one opaque element ---
{
    my $inner = Set.new(1, 2);
    my $b = Bag.new($inner);
    is $b.total, 1, 'a Set passed to Bag.new is a single element';
}
