use v6;
use lib 't/lib';
use Test;
use HookOperatorFamily;
need HookOperatorFamily::Ops;

plan 4;

# A block's own `multi` of an operator joins the family a `sub EXPORT` hook
# handed the unit: the narrower local candidate wins inside the block, as
# rakudo's derived dispatcher does, and the block's exit takes it away.
{
    multi infix:<+>(HookOperatorFamily::Ops::Thing $left, Int $right) { "local" }
    is Thing + 1, "local", 'a block multi wins over the hook family';
    my &c = { Thing + 1 };
    is c(), "local", 'a closure of the block sees the block multi';
    is (1,).map({ Thing + 1 }).List, ("local",), 'so does a map callback';
}
is Thing + 1, "imported", 'outside the block the hook family answers';
