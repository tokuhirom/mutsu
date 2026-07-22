use Test;

# `.tree` itemizes every node it descends into: an Iterable becomes
# `$(self.map(*.tree).Seq)`. mutsu used to return a plain, non-itemized Array,
# so `(1, 2).tree.raku` was `(1, 2)` instead of `$((1, 2).Seq)`. The `.tree(n)`
# depth forms itemize the same way, which is what makes `.tree(*)` and `.tree`
# identical (roast S02-lists/tree.t test 12).

plan 15;

is (1, 2).tree.raku, '$((1, 2).Seq)', '.tree itemizes the top node';
is (1, (2, 3)).tree.raku, '$((1, $((2, 3).Seq)).Seq)', '.tree itemizes every level';
is ((1, 2), (3,)).tree.raku, '$(($((1, 2).Seq), $((3,).Seq)).Seq)', '.tree over a list of lists';
is ().tree.raku, '$(().Seq)', '.tree on an empty list';
is [1, 2].tree.raku, '$((1, 2).Seq)', '.tree on an Array yields a Seq';
is (1..3).tree.raku, '$((1, 2, 3).Seq)', '.tree expands a Range';
is %(:a(1)).tree.raku, '$((:a(1),).Seq)', '.tree on a Hash trees its pairs';
is (1, 2).tree.WHAT.gist, '(Seq)', '.tree yields a Seq, not a List';

# A non-Iterable is its own tree.
is "ab".tree.raku, '"ab"', '.tree on a Str is the Str';
is 42.tree.raku, '42', '.tree on an Int is the Int';

# The depth forms itemize exactly as far as they descend.
is (1, 2).tree(0).raku, '(1, 2)', '.tree(0) is the identity';
is (1, (2, (3, 4))).tree(1).raku, '$((1, (2, (3, 4))).Seq)', '.tree(1) itemizes one level';
is (1, (2, (3, 4))).tree(2).raku, '$((1, $((2, (3, 4)).Seq)).Seq)', '.tree(2) itemizes two levels';
is (1, (2, 3)).tree(*).raku, (1, (2, 3)).tree.raku, '.tree(*) is .tree';

# The closure forms are unchanged.
is ((1, 2), (3, 4)).tree(*.join(' '), *.join('|')), '1|2 3|4', '.tree with Whatever-closures';

done-testing;
