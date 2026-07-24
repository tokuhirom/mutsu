use v6;
use Test;

# Set/Bag/Mix element identity is `.WHICH`-based (raku Setty semantics): 1,
# "1", 1.0 and <1> are four distinct elements, and two instances of the same
# class are distinct objects. The backing stores key by the element's `.WHICH`
# with the element objects preserved, so `.keys`/`.pairs`/`.gist`/`.raku`
# report the real objects. All expectations verified against the reference
# raku. (PLAN 8.10, the Set/Bag/Mix half of the object-hash campaign.)

plan 25;

# element identity across types
is (1, "1", 1.0, <1>).Set.elems, 4, 'Int/Str/Rat/IntStr are four distinct elements';
is-deeply (1, "1").Set.keys.map(*.^name).sort.list, ("Int", "Str"),
    'keys keep the element types';
is (1.0,).Set.keys[0].^name, 'Rat', 'a Rat element survives as Rat';

# membership is element identity (===), not stringification
ok 1 ∈ (1,).Set, 'Int 1 is a member';
nok "1" ∈ (1,).Set, 'Str "1" is not (Int elements)';
nok 1 ∈ ("1",).Set, 'Int 1 is not (Str elements)';
nok <1> ∈ (1,).Set, 'IntStr <1> is not (allomorph identity)';

# object elements: distinct instances are distinct elements
{
    class C {}
    my $c = C.new;
    is (C.new, C.new).Set.elems, 2, 'two instances are two elements';
    ok (($c,).Set){$c}, 'the same instance is found';
    nok (($c,).Set){C.new}, 'a different instance is not';
}

# Bag counting by identity
{
    my $b = (1, "1", 1, 1.0).Bag;
    is $b{1}, 2, 'Bag counts Int 1 twice';
    is $b{"1"}, 1, 'Str "1" counted separately';
    is $b{1.0}, 1, 'Rat 1.0 counted separately';
}

# Mix weights by identity (pair-list constructor)
{
    my $m = (1 => 0.5, "1" => 2).Mix;
    is $m.elems, 2, 'Int and Str keys stay distinct';
    is $m{1}, 0.5, 'Int weight';
    is $m{"1"}, 2, 'Str weight';
}

# set operators preserve element identity
is ((1,).Set ∪ ("1",).Set).elems, 2, 'union keeps both identities';
is-deeply ((1, "1").Set ∩ (1,).Set).keys.map(*.^name).list, ("Int",),
    'intersection keeps the Int element';
nok (1,).Set eqv ("1",).Set, 'eqv distinguishes Int from Str elements';
ok (1, "1").Set eqv (1, "1").Set, 'eqv on identical mixed sets';

# SetHash mutation by identity
{
    my $sh = SetHash.new;
    $sh.set(1);
    nok "1" ∈ $sh, '.set(Int) does not admit the Str';
    $sh.set("1");
    is $sh.elems, 2, '.set(Str) adds a second element';
    $sh.unset(1);
    is $sh.elems, 1, '.unset(Int) removes only the Int';
}

# rendering reports the element objects
is (1, "1").Set.gist, 'Set(1 1)', 'gist shows both elements';
ok (1, "1").Set.raku.EVAL eqv (1, "1").Set, 'raku round-trips';
