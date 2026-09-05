use Test;

# ADR-0041 section 9: a BEGIN-time region temporarily rolls hoisted-but-not-yet-reached
# sub declarations out of the routine registry, so a `&name` reference evaluated
# there sees only what the program has textually reached. If the region's
# closing opcode is skipped because the body threw and the throw was caught, the
# rollback must NOT survive into ordinary execution.
#
# NOTE: rakudo refuses to compile either program below outright ("An exception
# X::AdHoc occurred while evaluating a constant / a BEGIN") because it runs
# BEGIN during compilation; mutsu evaluates it at runtime, where `try` does
# catch it. The invariant under test is therefore mutsu-internal: whatever the
# region hid must be back afterwards.

plan 4;

{
    my $caught = False;
    try {
        constant DEAD = die "boom";
        CATCH { default { $caught = True } }
    }
    ok $caught, 'a throw out of a `constant` initializer is caught';
    is after-dead-constant(), "reachable",
       'a sub declared after a caught BEGIN-time throw is still callable';
    sub after-dead-constant() { "reachable" }
}

{
    my $caught = False;
    try {
        BEGIN { die "boom" }
        CATCH { default { $caught = True } }
    }
    ok $caught, 'a throw out of a BEGIN block is caught';
    is after-dead-begin(), "reachable",
       'a sub declared after a caught BEGIN-block throw is still callable';
    sub after-dead-begin() { "reachable" }
}
