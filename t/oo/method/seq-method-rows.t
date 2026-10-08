use Test;

# A settled Seq (reified, not lazy) answers through the Seq rows of the built-in
# method table (ADR-11276, remainder: the Seq shape). A method that consumes a Seq
# still consumes it, because the rows run after the consumption step.

plan 54;

sub mk { (1, 2, 3).map(* + 1) }

# Non-consuming reads
is mk().elems, 3, 'Seq.elems';
is mk().end, 2, 'Seq.end';
is mk().Bool, True, 'Seq.Bool';
is (().Seq).Bool, False, 'an empty Seq is false';
is mk().is-lazy, False, 'Seq.is-lazy';
is mk().AT-POS(1), 3, 'Seq.AT-POS';
nok mk().AT-POS(9).defined, 'Seq.AT-POS past the end';
is mk().EXISTS-POS(2), True, 'Seq.EXISTS-POS';
is mk().EXISTS-POS(5), False, 'Seq.EXISTS-POS past the end';
is mk().head, 2, 'Seq.head';
is mk().head(2).join(','), '2,3', 'Seq.head(2)';
is mk().join, '234', 'Seq.join';
is mk().join('-'), '2-3-4', 'Seq.join with a separator';
is mk().Capture.list.join(','), '2,3,4', 'Seq.Capture';
is mk().flat.join(','), '2,3,4', 'Seq.flat';
is mk().reverse.join(','), '4,3,2', 'Seq.reverse';
isa-ok mk().reverse, Seq, 'Seq.reverse is a Seq';
is mk().sink, Nil, 'Seq.sink';

# Coercions
isa-ok mk().List, List, 'Seq.List is a List';
is mk().List.join(','), '2,3,4', 'Seq.List elements';
isa-ok mk().list, List, 'Seq.list is a List';
isa-ok mk().Array, Array, 'Seq.Array is an Array';
is mk().Array.join(','), '2,3,4', 'Seq.Array elements';
isa-ok mk().Slip, Slip, 'Seq.Slip is a Slip';
is mk().Slip.elems, 3, 'Seq.Slip elements';
isa-ok mk().cache, List, 'Seq.cache is a List';
is mk().cache.join(','), '2,3,4', 'Seq.cache elements';
isa-ok mk().item, Seq, 'Seq.item keeps the Seq';
isa-ok mk().hyper, HyperSeq, 'Seq.hyper';
isa-ok mk().race, RaceSeq, 'Seq.race';
isa-ok mk().lazy, Seq, 'Seq.lazy is a Seq';
ok mk().lazy.is-lazy, 'Seq.lazy is lazy';

# A touch that reads the Seq keeps it readable; a consuming one does not.
{
    my $s = mk();
    is $s.elems, 3, 'first read';
    is $s.elems, 3, 'a second read of the same Seq';
    is $s.join(','), '2,3,4', 'join after elems';
    is $s.join(','), '2,3,4', 'join twice';
}
{
    my $s = (1, 2, 3).Seq;
    is $s.reverse.join(','), '3,2,1', 'a fresh Seq reverses once';
    throws-like { $s.reverse }, X::Seq::Consumed, 'and is consumed by it';
}
{
    my $s = (1, 2, 3).Seq;
    is $s.head(2).join(','), '1,2', 'a fresh Seq heads once';
    throws-like { $s.head(2) }, X::Seq::Consumed, 'and is consumed by it';
}
{
    my $s = (1, 2, 3).Seq;
    is $s.flat.join(','), '1,2,3', 'a fresh Seq flattens once';
    throws-like { $s.flat }, X::Seq::Consumed, 'and is consumed by it';
}
{
    my $s = (1, 2, 3).Seq;
    $s.elems;
    is $s.List.join(','), '1,2,3', 'List after a read';
}
{
    my $s = (1, 2, 3).Seq;
    my $c = $s.cache;
    is $c.elems, 3, 'the cached List';
    is $s.Slip.elems, 3, 'Slip after cache';
}

# Receivers the shape does not cover keep their behaviour.
{
    my $lazy = (1 .. *).Seq;
    ok $lazy.is-lazy, 'an infinite Seq is lazy';
    is $lazy.head(3).join(','), '1,2,3', 'head of an infinite Seq';
    my $gen = gather { take 1; take 2 };
    is $gen.elems, 2, 'a gather Seq';
    is $gen.join(','), '1,2', 'a gather Seq reads again';
}
is (1, (2, 3)).Seq.flat.join(','), '1,2,3', 'flat flattens one level of a nested Seq';
is (<a b> X <1 2>).Seq.join(','), 'a 1,a 2,b 1,b 2', 'a cross-product Seq';
is "abc".comb.Seq.elems, 3, 'a Seq from a string';

# A user class that is a Seq-like keeps its own methods.
class Mine { method elems { 42 }; method head { 'mine' } }
is Mine.new.elems, 42, 'a user class overrides elems';
is Mine.new.head, 'mine', 'a user class overrides head';
