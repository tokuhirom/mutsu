use v6;
use lib 't/lib';
use Test;
use BlockOperatorImport;

plan 5;

# A block's narrower `multi infix:<+>` wins over the imported candidate also
# inside a map callback, which mutsu compiles again as a loop body.
{
    multi infix:<+>(Box $left, Int $right) { "local" }
    is Box + 1, "local", 'the block multi answers in the block';
    is (1, 2).map({ Box + 1 }).List, <local local>, 'and in a map callback';
}

sub run-block(&b) { b() }

# A bare block in tail position of a closure is a scope of its own: the
# routine it declares does not outlive it.
run-block {
    {
        multi infix:<+>(Box $left, Int $right) { "first" }
        is Box + 1, "first", 'the tail block multi answers in its block';
    }
};
{
    multi infix:<+>(Box $left, Int $right) { "second" }
    is Box + 1, "second", 'a sibling block declares the same candidate afresh';
}
is Box + 1, "imported", 'outside the blocks the imported candidate answers';
