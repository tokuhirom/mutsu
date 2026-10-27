use Test;

plan 5;

# `>>.method` on an object whose class does Iterable walks its iterator and
# calls the method on each element (rakudo's deepmap reads `self.iterator`),
# instead of calling the method on the object itself.
class P { has $.id; }
class Seqy does Iterable {
    has @.items;
    method iterator { @!items.iterator }
}
class Holder {
    has @.ids;
    method rows { Seqy.new(items => @!ids.map({ P.new(id => $_) })) }
}

my $s = Seqy.new(items => (P.new(id => 1), P.new(id => 2)));
is $s>>.id.join(' '), '1 2', '>>.id on an Iterable object maps over its elements';
is $s.hyper.map(*.id).join(' '), '1 2', 'baseline: the elements are reachable';

my $h = Holder.new(ids => (3, 4));
is $h.rows>>.id.join(' '), '3 4', 'a method returning the Iterable object';
is (Holder.new(ids => (5, 6)),).Seq.map(|*.rows>>.id).join(' '), '5 6',
    'inside a slipping WhateverCode';
is Seqy.new(items => ())>>.id.elems, 0, 'an empty Iterable maps to nothing';
