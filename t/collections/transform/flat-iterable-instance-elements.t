use Test;

plan 11;

class It does Iterator {
    has $.n is rw = 0;
    method pull-one { $!n < 3 ?? $!n++ !! IterationEnd }
}
class C does Iterable { method iterator { It.new } }
class Q is Range { }
class H is Seq { }

is (C.new, 9).flat.List.raku, (0, 1, 2, 9).raku, '.flat reads the iterator of an Iterable element';
is (Q.new(1, 3), 9).flat.List.raku, (1, 2, 3, 9).raku, '.flat flattens a Range subclass element';
is (H.new(It.new), 9).flat.List.raku, (0, 1, 2, 9).raku, '.flat flattens a Seq subclass element';
is flat(C.new, 9).List.raku, (0, 1, 2, 9).raku, 'flat() reads the iterator of an Iterable argument';
is ((1, (C.new, 2)), 3).flat.List.raku, (1, 0, 1, 2, 2, 3).raku, 'a nested list is descended into';
is [C.new, 9].flat.elems, 2, 'an Array element is an item and stays whole';

is ([+] Q.new(1, 3)), 6, '[+] over a Range subclass folds its elements';
is ([,] Q.new(1, 3)).raku, (1, 2, 3).raku, '[,] over a Range subclass lists its elements';
is ([max] Q.new(1, 3)), 3, '[max] over a Range subclass';
my $q = Q.new(1, 3);
is ([+] $q), 3, 'a $ variable holding it is one item';
my $s = (1, 2, 3).Seq;
is ([+] $s), 6, 'a $-held Seq is still iterated';
