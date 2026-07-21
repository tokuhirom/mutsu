use Test;

plan 4;

# `.^parents(:tree)` returns the parent tree. A node with a single parent is
# `[node, subtree]`; a node with multiple parents is `[node, (sub1, sub2, ...)]`
# with the children in a List; a leaf is `[Mu]`. The top level unwraps a single
# parent's subtree, or returns a List of subtrees for multiple parents. `:all`
# does not change the tree. Previously mutsu flattened multi-parent children and
# wrapped the whole result in an extra array.

class D { };
class C1 is D { };
class C2 is D { };
class B is C1 is C2 { };
class A is B { };

is A.^parents(:all, :tree).raku,
    '[B, ([C1, [D, [Any, [Mu]]]], [C2, [D, [Any, [Mu]]]])]',
    'diamond tree: multi-parent children are a List, top single parent unwrapped';

class X { };
class Y is X { };
is Y.^parents(:all, :tree).raku, '[X, [Any, [Mu]]]',
    'single-parent chain';

class P1 { };
class P2 { };
class M is P1 is P2 { };
is M.^parents(:all, :tree).raku, '([P1, [Any, [Mu]]], [P2, [Any, [Mu]]])',
    'multiple parents at the top yield a List of subtrees';

class Z { };
is Z.^parents(:all, :tree).raku, '[Any, [Mu]]',
    'a base class tree is just its Any/Mu chain';
