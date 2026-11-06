use Test;

plan 4;

# `Any.Seq` is `Seq.new(self.iterator)`: a type object of a class that defines
# `iterator` reads it, defined or not.
class Pair2 { method iterator { (7, 8).iterator } }
is-deeply Pair2.Seq.list, (7, 8), 'type object .Seq goes through the class iterator';
is-deeply Pair2.new.Seq.list, (7, 8), 'and so does an instance';

class Empty does Iterable {
    method iterator { ().iterator }
    method list() { self.Seq.list }
}
my Empty $e;
is-deeply $e.list, (), 'an uninitialized typed scalar lists as empty';
is-deeply Int.Seq.list, (Int,), 'a core type object is still a one-element Seq';
