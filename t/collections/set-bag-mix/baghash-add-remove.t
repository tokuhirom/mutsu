use v6;
use Test;

plan 44;

# rakudo declares `add`/`remove` on BagHash itself (not on the Baggy role),
# so BagHash is the only QuantHash that has them.

# --- basic increment / decrement --------------------------------------------
{
    my $n = BagHash.new: "a", "b", "c", "c";
    is $n<c>, 2, 'initial count';
    $n.add('c');
    is $n<c>, 3, '.add increments an existing key';
    $n.add('d');
    is $n<d>, 1, '.add inserts an absent key at 1';
    $n.remove('c');
    is $n<c>, 2, '.remove decrements an existing key';
    $n.remove('a');
    is $n<a>, 0, '.remove drops a key whose count reaches 0';
    nok ($n<a>:exists), 'the dropped key is gone, not stored at 0';
    is $n.elems, 3, 'elems after the drop';
}

# --- remove is saturating at zero -------------------------------------------
{
    my $z = BagHash.new: "a";
    $z.remove('nope');
    is $z.elems, 1, '.remove of an absent key is a no-op';
    nok ($z<nope>:exists), 'no negative-count key is created';
    $z.remove('a');
    $z.remove('a');
    is $z<a>, 0, '.remove below zero stays at zero';
    is $z.elems, 0, 'the bag is empty';
}

# --- both return Nil --------------------------------------------------------
{
    my $r = BagHash.new: "a";
    ok $r.add('a') === Nil, '.add returns Nil';
    ok $r.remove('a') === Nil, '.remove returns Nil';
}

# --- the argument is iterated one level -------------------------------------
{
    my $q = BagHash.new;
    $q.add(('x', 'y', 'y'));
    is $q<x>, 1, 'a list argument adds each element (x)';
    is $q<y>, 2, 'a list argument adds each element, duplicates counting twice';

    my @a = <m n n>;
    $q.add(@a);
    is $q<n>, 2, 'an Array argument is iterated too';

    my $rg = BagHash.new;
    $rg.add(1..3);
    is $rg.total, 3, 'a Range argument is iterated';
    is $rg{2}, 1, 'a Range element lands under its own (Int) key';

    my $sq = BagHash.new;
    $sq.add((1, 2, 2).Seq);
    is $sq{2}, 2, 'a Seq argument is iterated';
}

{
    # No deep flattening: an inner list is a single element of its own.
    my $nested = BagHash.new;
    $nested.add(('a', ('b', 'b')));
    is $nested<a>, 1, 'nested list: the outer scalar element is added';
    is $nested.elems, 2, 'nested list: the inner list is ONE element, not flattened';
}

{
    # A Pair is a single element (it is not Iterable), so it becomes a key.
    my $p = BagHash.new;
    $p.add('c' => 3);
    is $p.elems, 1, 'a Pair argument is a single element';
    is $p{'c' => 3}, 1, 'the Pair itself is the key, with count 1';
    is $p<c>, 0, 'the Pair is NOT interpreted as a key => weight entry';
}

{
    # A Baggy argument iterates as its `key => weight` pairs.
    my $r = BagHash.new;
    $r.add(bag(<u v v>));
    is $r.elems, 2, 'a Bag argument yields one element per pair';
    is $r{'v' => 2}, 1, 'the pair v => 2 is the key';
}

# --- an Int / Str argument is a single element ------------------------------
{
    my $i = BagHash.new;
    $i.add(42);
    is $i{42}, 1, 'an Int argument is one element';
    $i.remove(42);
    is $i.elems, 0, 'and .remove takes it back out';
}

# --- remove over a list -----------------------------------------------------
{
    my $n = BagHash.new: "a", "b", "c", "c";
    $n.remove(('b', 'a'));
    is $n.elems, 1, '.remove over a list drops every listed key';
    is $n<c>, 2, 'untouched keys keep their count';
    $n.remove(('c', 'c'));
    is $n<c>, 0, 'a duplicated element decrements twice';
}

# --- mutation is shared with every alias ------------------------------------
{
    my $a = BagHash.new: 'x';
    my $b = $a;
    $a.add('x');
    is $b<x>, 2, '.add through one alias is visible through the other';
    sub bump($c) { $c.remove('x') }
    bump($a);
    is $b<x>, 1, '.remove through a sub parameter is visible at the caller';
}

# --- every invocant shape, not just a plain `$` variable --------------------
{
    my %bh is BagHash = <a b b>;
    %bh.add('c');
    is %bh<c>, 1, '.add on a `%`-sigil BagHash variable';
    %bh.remove('b');
    is %bh<b>, 1, '.remove on a `%`-sigil BagHash variable';
    is %bh<c>, 1, 'the earlier .add is not resurrected by a stale snapshot';

    class HasBag { has $.bag = BagHash.new('q'); }
    my $o = HasBag.new;
    $o.bag.add('q');
    is $o.bag<q>, 2, '.add on an attribute (no variable to write back to)';

    my @bags = BagHash.new('p'), BagHash.new('r');
    @bags[0].add('p');
    is @bags[0]<p>, 2, '.add on an array element';

    my $rep = BagHash.new: 't';
    $rep.add('t') for ^3;
    is $rep<t>, 4, 'repeated .add in a loop accumulates';
}

# --- the immutable / non-Baggy types do NOT get these methods ---------------
{
    dies-ok { bag(<a b>).add('a') }, 'Bag has no .add';
    dies-ok { bag(<a b>).remove('a') }, 'Bag has no .remove';
    dies-ok { (a => 1.5).Mix.add('a') }, 'Mix has no .add';
    dies-ok { MixHash.new('a').add('a') }, 'MixHash has no .add (rakudo puts add on BagHash, not Baggy)';
    dies-ok { SetHash.new('a').add('a') }, 'SetHash has no .add';
}
