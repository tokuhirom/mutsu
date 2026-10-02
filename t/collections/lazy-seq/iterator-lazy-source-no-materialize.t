use Test;

plan 16;

# `.iterator` on a lazy source (an unbounded Range, a lazy pipe, a gather)
# used to materialize the source into the Iterator up front: a 1M-element
# prefix for `1..*`, after which `pull-one` answered IterationEnd as though
# the infinite source had ended (#10782). The iterator now pulls from the
# source on demand, so building it is O(1) and there is no cap.

{
    my $i = (1.5..*).iterator;
    is $i.pull-one, 1.5, 'Rat-start unbounded range: first element';
    is $i.pull-one, 2.5, 'Rat-start unbounded range: second element';
}

{
    my $i = ("a"..*).iterator;
    is ($i.pull-one xx 3).join(','), 'a,b,c', 'Str-start unbounded range steps by .succ';
}

{
    my $i = (1..*).map(* + 1).iterator;
    is $i.pull-one, 2, 'lazy map pipe: first element';
    is $i.pull-one, 3, 'lazy map pipe: second element';
}

{
    my $i = (0..*).iterator;
    $i.skip-at-least(1_500_000);
    is $i.pull-one, 1_500_000, 'pulling past the old 1M reification cap keeps going';
}

{
    my $i = (^Inf).iterator;
    my @out;
    $i.push-exactly(@out, 3);
    is-deeply @out, [0, 1, 2], 'push-exactly pulls only what it was asked for';
    is $i.pull-one, 3, 'and the cursor continues after it';
}

{
    my $start = now;
    my $i = (1..*).iterator for ^200;
    ok now - $start < 5, 'building many unbounded-range iterators is cheap';
}

{
    my $i = (1..*).iterator;
    ok $i.is-lazy, 'an unbounded-range iterator is lazy';
    is-deeply $i.can('count-only').elems, 0, 'it cannot predict a count';
    throws-like { $i.count-only }, X::Method::NotFound,
        'count-only is not available, as in Rakudo';
}

is Seq.new((1..*).iterator).head(3).join(','), '1,2,3',
    'Seq.new over a lazy-source iterator pulls on demand';

{
    my $i = (gather { take 1; take 2; take 3 }).iterator;
    $i.pull-one;
    is-deeply List.from-iterator($i), (2, 3), 'from-iterator drains the rest of a lazy source';
}

{
    my $i = (lazy 1..3).iterator;
    my @out;
    $i.push-all(@out);
    is-deeply @out, [1, 2, 3], 'push-all drains a finite lazy source';
}

is-deeply (1..5).iterator.count-only, 5, 'a bounded range iterator still predicts its count';
