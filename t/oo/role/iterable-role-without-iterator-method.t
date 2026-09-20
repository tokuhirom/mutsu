use Test;

# A class that composes `Iterable` without declaring its own `iterator`
# inherits `Any.iterator`, which Rakudo defines as `self.list.iterator`. So
# `does Iterable` plus a `list` method is enough to decompose — mutsu used to
# require an explicit `iterator` method and yielded the instance itself as a
# single element.
#
# Came from Selkie::UI (ecosystem): `Selkie::UI::ReactiveArray` is
# `does Positional does Iterable` and provides `list`/`AT-POS`/`elems` only,
# so `for @a` over one iterated the object instead of its three elements.

plan 6;

class Reactiveish does Positional does Iterable {
    has @.data;
    method list      { @!data.List }
    method AT-POS($i) { @!data.AT-POS($i) }
    method elems     { @!data.elems }
}

my @bound := Reactiveish.new(data => [1, 2, 3]);

my @collected;
for @bound -> $e { @collected.push: $e }
is-deeply @collected, [1, 2, 3], '`for` over a bound Iterable-by-list object yields its elements';

my @assigned = Reactiveish.new(data => [4, 5]);
is-deeply @assigned, [4, 5], '`@`-assignment of an Iterable-by-list object decomposes';

my $obj = Reactiveish.new(data => [1, 2, 3]);
is-deeply $obj.map({ $_ * 2 }).List, (2, 4, 6),
    '.map on an Iterable-by-list object iterates its elements';
is-deeply $obj.grep(* > 1).List, (2, 3),
    '.grep on an Iterable-by-list object iterates its elements';

# A scalar still holds the object whole: itemization wins over decomposition.
my @itemized = $obj;
is @itemized.elems, 1, 'a scalar-held Iterable-by-list object stays one element';

# A plain class with a `list` method but no `Iterable` is NOT iterable.
class PlainList {
    has @.data;
    method list { @!data.List }
}
my @plain = PlainList.new(data => [1, 2, 3]);
is @plain.elems, 1, 'a `list` method alone does not make a class decompose';
