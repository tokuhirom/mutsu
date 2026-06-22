use v6;
use Test;

plan 7;

# A `constant` is lexically scoped: a nested declaration of the same name is a
# fresh binding that shadows the outer one only within its own scope, and must
# not leak back to (or clobber) the outer constant.

# Shadow inside a bare block.
{
    constant T = "outer";
    {
        constant T = "inner";
        is T, "inner", "inner block sees its own shadowing constant";
    }
    is T, "outer", "outer scope keeps its constant after a shadowing block";
}

# Shadow inside a named sub; a sibling sub must still see the outer constant.
{
    constant U = "outer";
    sub with-shadow() { constant U = "inner"; U }
    sub no-shadow() { U }
    is with-shadow(), "inner", "sub with a shadowing constant sees the shadow";
    is no-shadow(), "outer", "sibling sub sees the outer constant, not the shadow";
    is U, "outer", "outer scope unaffected by a sub-local shadowing constant";
}

# A constant declared at the top of a scope is visible to a sub defined later
# (lexical visibility must survive the shadow fix).
{
    constant V = "visible";
    sub reader() { V }
    is reader(), "visible", "constant is visible inside a later-defined sub";
}

# A nested sub captures the *shadowing* constant of its enclosing block.
{
    constant W = "outer";
    {
        constant W = "inner";
        sub deep() { W }
        is deep(), "inner", "nested sub captures the enclosing shadowing constant";
    }
}
