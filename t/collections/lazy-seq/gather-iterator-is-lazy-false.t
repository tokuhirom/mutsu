use v6;
use Test;

# Grammar::Extractor: a class whose `iterator` is a gather's iterator is not
# lazy, so `.grep`/`.map` on it give a reified Seq that `.Array.Bag` accepts.

plan 6;

ok !(gather { take 1; take 2 }).iterator.is-lazy, 'a gather iterator is not lazy';
ok (1..*).iterator.is-lazy, 'an infinite range iterator is';

class S {
    has @.kids = 1, 2, 3;
    method Seq { gather { for @!kids { .take } } }
    method iterator { $.Seq.iterator }
    method matches { @.grep(* > 1).map(* * 2).Array.Bag }
}
my $s = S.new;
nok $s.grep(* > 1).is-lazy, '.grep over its own gather iterator is not lazy';
nok $s.map(* + 1).is-lazy, '.map over it is not lazy';
is $s.matches, Bag.new(4, 6), '.Array.Bag works on the mapped result';
is $s.grep(* > 1).elems, 2, '.elems works';
