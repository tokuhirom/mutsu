use Test;

plan 8;

# `Iterable`'s hyper/race/lazy read a user `Iterable` through its `iterator`.
class It does Iterable { method iterator { (1, 2, 3).iterator } }

is-deeply It.new.hyper.List, (1, 2, 3), 'hyper reads the iterator';
is-deeply It.new.race.List, (1, 2, 3), 'race reads the iterator';
ok It.new.lazy.is-lazy, 'lazy is lazy';
is-deeply It.new.lazy.head(2).List, (1, 2), 'lazy yields the iterator elements';
is It.new.race.sum, 6, 'race.sum';
is It.new.hyper.elems, 3, 'hyper.elems';

# `Any.List` of a user `Iterable` is one element in Rakudo too.
is It.new.List.elems, 1, '.List of a user Iterable is a one-element list';

# A class that is not Iterable has no hyper.
class Plain { method iterator { (1, 2, 3).iterator } }
throws-like { Plain.new.hyper }, X::Method::NotFound, 'a non-Iterable still has no hyper';
