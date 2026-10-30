use Test;

# annotations' `CALL-ME(::?CLASS: +offsets) { ANN[$?CLASS][offsets] }` called as
# `Package(*)`: a Whatever subscript on a user class that provides its own
# `AT-POS` answered Nil (the Whatever was handed to `AT-POS(Int:D)`).

plan 7;

class P does Positional {
    method elems { 3 }
    method AT-POS(Int:D $i) is raw { $i * 10 }
}
my @p := P.new;

is @p[*].elems, 1, 'a lone Whatever is the object\'s .list (Any.list: the object itself)';
is-deeply @p[(*,)], ((0, 10, 20),), 'a Whatever inside a slice reads every element as a list';
is-deeply @p[*, 1], ((0, 10, 20), 10), 'mixed with an index';
is-deeply @p[0, 1], (0, 10), 'plain slices are unchanged';
is @p[*-1], 20, 'a WhateverCode is unchanged';

class L does Positional {
    method elems { 3 }
    method AT-POS(Int:D $i) is raw { $i * 10 }
    method list { (7, 8) }
}
my @l := L.new;
is-deeply @l[*], (7, 8), 'a lone Whatever calls the class\'s own list';
sub offsets(+o) { @l[o] }
is-deeply offsets(*), ((0, 10, 20),), 'a Whatever collected by +o is a slice element';
