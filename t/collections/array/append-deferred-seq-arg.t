use Test;

# From ML::SparseMatrixRecommender (Math::SparseMatrix::CSR.row-bind):
# append/prepend on a receiver with no variable name, given a deferred
# `.map` Seq, used to add nothing.
plan 5;

is-deeply [0, 1].append((1, 2).map({ $_ + 1 })).Array, [0, 1, 2, 3], 'append of a deferred map Seq';
is-deeply [0, 1].prepend((1, 2).map({ $_ + 1 })).Array, [2, 3, 0, 1], 'prepend of a deferred map Seq';

my @x = 0, 1;
is-deeply @x.clone.append((1, 2).map({ $_ + 1 })).Array, [0, 1, 2, 3], 'clone.append of a map Seq';
is-deeply @x.clone.Array.append(<a b>.grep({ True })).Array, [0, 1, "a", "b"], 'append of a grep Seq';

@x.append((5, 6).map({ $_ * 2 }));
is-deeply @x, [0, 1, 10, 12], 'append on a named array still works';
