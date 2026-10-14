# A user class that composes `Iterable` coerces to a Set/Bag/Mix, and takes
# part in a set operator, through its elements, as rakudo's `Any.Set` and the
# set operators read `self.iterator`. Found in ValueList's t/01-basic.rakutest
# (`ok @a.roll (elem) @a`).
use Test;

plan 11;

class It does Iterable {
    method iterator() { (1, 2, 3).iterator }
}
class Li does Iterable {
    method list() { (7, 8).List }
}
class OnlyIter {
    method iterator() { (1, 2, 3).iterator }
}

is It.new.Set.keys.sort.join(','), '1,2,3', '.Set iterates';
is It.new.Bag.keys.sort.join(','), '1,2,3', '.Bag iterates';
is It.new.Mix.keys.sort.join(','), '1,2,3', '.Mix iterates';
ok 2 (elem) It.new, '(elem) on an Iterable instance';
ok It.new (cont) 3, '(cont) on an Iterable instance';
nok 9 ∈ It.new, 'a missing element';
is Li.new.Set.keys.sort.join(','), '7,8', '.Set through a `list` method';
ok 7 ∈ Li.new, '∈ through a `list` method';
is (Li.new ∪ (1,)).keys.sort.join(','), '1,7,8', 'union';
is OnlyIter.new.Set.elems, 1, 'without Iterable the instance is one element';
nok 2 ∈ OnlyIter.new, 'and is not searched';
