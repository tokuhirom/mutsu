use Test;

plan 4;

# A submethod and an `our sub` of the same name live in different
# namespaces: neither is a redeclaration of the other, in either order.
class SubFirst {
    our sub make($v) { "sub $v" }
    submethod make($v) { "submethod $v" }
}
class SubmethodFirst {
    submethod make($v) { "submethod $v" }
    our sub make($v) { "sub $v" }
}

is SubFirst::make(1), 'sub 1', 'our sub declared before the submethod';
is SubFirst.make(2), 'submethod 2', 'submethod declared after the our sub';
is SubmethodFirst::make(3), 'sub 3', 'our sub declared after the submethod';
is SubmethodFirst.make(4), 'submethod 4', 'submethod declared before the our sub';
