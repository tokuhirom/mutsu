use Test;

# `.all`/`.any`/`.one`/`.none` on an object that does Iterable read its iterator, as
# rakudo's Any.all does (the `.list` of an Iterable-only object is the object itself).
# An Iterable-and-Positional object is iterated too; a Positional-only one is listed.

plan 9;

class It does Iterable {
    method iterator { (4, 5).iterator }
}
is It.new.all.raku, 'all(4, 5)', 'all over an Iterable-only object';
is It.new.any.raku, 'any(4, 5)', 'any';
is It.new.one.raku, 'one(4, 5)', 'one';
is It.new.none.raku, 'none(4, 5)', 'none';

ok It.new.all > 3, 'and it junctions as a value';

class Pos does Positional {
    method list { (1, 2) }
    method AT-POS($i) { $i }
    method elems { 2 }
}
is Pos.new.all.raku, 'all(1, 2)', 'a Positional-only object is listed';

class Both does Iterable does Positional {
    method list { (7, 8) }
    method iterator { (1, 2).iterator }
    method AT-POS($i) { $i }
    method elems { 2 }
}
is Both.new.all.raku, 'all(1, 2)', 'an Iterable and Positional object is iterated';

class Plain { has $.x = 1 }
is Plain.new.all.elems, 1, 'a plain object stays one item';
is (1, 2, 3).all.raku, 'all(1, 2, 3)', 'a list is unchanged';
